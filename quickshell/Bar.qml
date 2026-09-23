import QtQuick
import QtQuick.Layouts
import Quickshell
import Quickshell.Hyprland
import Quickshell.Io
import Quickshell.Networking
import Quickshell.Services.Mpris
import Quickshell.Services.Pipewire
import Quickshell.Services.SystemTray
import Quickshell.Widgets

PanelWindow {
    id: bar

    required property ShellScreen modelData
    screen: modelData

    anchors {
        top: true
        left: true
        right: true
    }
    margins {
        top: 8
        left: 8
        right: 8
    }
    implicitHeight: 30
    color: "transparent"

    component Pill: Rectangle {
        property alias text: label.text
        property alias label: label
        property int maxTextWidth: 100000

        Layout.fillHeight: true
        implicitWidth: label.width + 14
        radius: 8
        opacity: 0.9
        color: Wal.color10
        border {
            width: 2
            color: Wal.color0
        }

        Text {
            id: label
            anchors.centerIn: parent
            width: Math.min(implicitWidth, parent.maxTextWidth)
            elide: Text.ElideRight
            color: Wal.color0
            font {
                family: Wal.font
                pixelSize: 13
            }
        }
    }

    RowLayout {
        anchors {
            left: parent.left
            top: parent.top
            bottom: parent.bottom
        }
        spacing: 8

        Pill {
            implicitWidth: workspaces.implicitWidth + 8

            Row {
                id: workspaces
                anchors.centerIn: parent
                spacing: 2
                height: parent.height - 4

                Repeater {
                    model: Hyprland.workspaces

                    Rectangle {
                        required property HyprlandWorkspace modelData

                        visible: modelData.id > 0 // hide special workspaces (pypr scratchpads)
                        width: Math.max(ws.implicitWidth + 14, 24)
                        height: parent.height
                        radius: 6
                        color: modelData.focused || wsMouse.containsMouse ? Wal.color14 : "transparent"

                        Text {
                            id: ws
                            anchors.centerIn: parent
                            text: parent.modelData.name
                            color: Wal.color0
                            font.family: Wal.font
                            font.pixelSize: 13
                        }

                        MouseArea {
                            id: wsMouse
                            anchors.fill: parent
                            hoverEnabled: true
                            onClicked: parent.modelData.activate()
                        }
                    }
                }
            }
        }

        Pill {
            readonly property MprisPlayer player: Mpris.players.values.find(p => p.identity.toLowerCase().includes("spotify")) ?? null
            readonly property string track: player ? `${player.trackArtist} - ${player.trackTitle}` : ""

            visible: player !== null
            text: " " + (track.length > 40 ? track.slice(0, 39) + "…" : track)

            MouseArea {
                anchors.fill: parent
                onClicked: parent.player.togglePlaying()
            }
        }
    }

    Pill {
        id: title

        // Hyprland (lua config) only reports the active window after the first focus change
        property string initial: ""

        anchors {
            horizontalCenter: parent.horizontalCenter
            top: parent.top
            bottom: parent.bottom
        }
        visible: text !== ""
        text: Hyprland.activeToplevel?.title ?? initial
        maxTextWidth: bar.width / 3
        label.font.family: "FiraCode Nerd Font"
        label.font.bold: true

        Connections {
            target: Hyprland
            function onActiveToplevelChanged() {
                title.initial = "";
            }
        }

        Process {
            running: true
            command: ["hyprctl", "activewindow", "-j"]
            stdout: StdioCollector {
                onStreamFinished: {
                    try {
                        title.initial = JSON.parse(text).title ?? "";
                    } catch (e) {} // no focused window
                }
            }
        }
    }

    RowLayout {
        anchors {
            right: parent.right
            top: parent.top
            bottom: parent.bottom
        }
        spacing: 8

        Pill {
            id: volume
            readonly property PwNode sink: Pipewire.defaultAudioSink
            readonly property PwNode source: Pipewire.defaultAudioSource
            readonly property int vol: Math.round((sink?.audio?.volume ?? 0) * 100)

            PwObjectTracker {
                objects: [volume.sink, volume.source]
            }

            text: {
                const icons = ["", "", ""];
                const out = sink?.audio?.muted ? "" : `${vol}% ${icons[Math.min(2, Math.floor(vol / 34))]}`;
                const src = source?.audio?.muted ? "" : `${Math.round((source?.audio?.volume ?? 0) * 100)}% `;
                return `${out} ${src}`;
            }

            MouseArea {
                anchors.fill: parent
                onClicked: Quickshell.execDetached(["pavucontrol"])
                onWheel: wheel => {
                    const audio = volume.sink?.audio;
                    if (audio)
                        audio.volume = Math.max(0, audio.volume + (wheel.angleDelta.y > 0 ? 0.01 : -0.01));
                }
            }
        }

        Pill {
            readonly property bool online: Networking.devices.values.some(d => d.connected)
            text: online ? "Online 🟢" : "Offline 🔴"
        }

        Pill {
            id: cpu
            property real lastIdle: 0
            property real lastTotal: 0
            property int usage: 0
            text: `${usage}% `

            FileView {
                id: stat
                path: "/proc/stat"
                onLoaded: {
                    const f = text().split("\n")[0].trim().split(/\s+/).slice(1).map(Number);
                    const idle = f[3] + f[4];
                    const total = f.reduce((a, b) => a + b, 0);
                    if (cpu.lastTotal > 0 && total > cpu.lastTotal)
                        cpu.usage = Math.round(100 * (1 - (idle - cpu.lastIdle) / (total - cpu.lastTotal)));
                    cpu.lastIdle = idle;
                    cpu.lastTotal = total;
                }
            }
        }

        Pill {
            id: mem
            property int usage: 0
            text: `${usage}% `

            FileView {
                id: meminfo
                path: "/proc/meminfo"
                onLoaded: {
                    const kb = key => Number(text().match(new RegExp(`${key}:\\s+(\\d+)`))[1]);
                    mem.usage = Math.round(100 * (1 - kb("MemAvailable") / kb("MemTotal")));
                }
            }
        }

        Timer {
            running: true
            repeat: true
            triggeredOnStart: true
            interval: 2000
            onTriggered: {
                stat.reload();
                meminfo.reload();
            }
        }

        Pill {
            id: language
            text: "…"

            Connections {
                target: Hyprland
                function onRawEvent(event) {
                    if (event.name === "activelayout")
                        language.text = event.data.split(",").pop();
                }
            }

            Process {
                running: true
                command: ["hyprctl", "devices", "-j"]
                stdout: StdioCollector {
                    onStreamFinished: {
                        const kbd = JSON.parse(text).keyboards.find(k => k.main);
                        if (kbd)
                            language.text = kbd.active_keymap;
                    }
                }
            }
        }

        Pill {
            property bool showDate: false
            text: Qt.formatDateTime(clock.date, showDate ? "dd-MM-yyyy" : "hh:mm")

            SystemClock {
                id: clock
                precision: SystemClock.Minutes
            }

            MouseArea {
                anchors.fill: parent
                onClicked: parent.showDate = !parent.showDate
            }
        }

        Pill {
            visible: SystemTray.items.values.length > 0
            implicitWidth: tray.implicitWidth + 14

            Row {
                id: tray
                anchors.centerIn: parent
                spacing: 10

                Repeater {
                    model: SystemTray.items

                    IconImage {
                        id: trayIcon
                        required property SystemTrayItem modelData
                        // steam/dropbox send "name?path=<dir>", which the icon loader can't resolve; try the usual spots
                        readonly property var sources: {
                            const [name, dir] = modelData.icon.split("?path=");
                            const file = name.split("/").pop() + ".png";
                            return dir ? [`file://${dir}/${file}`, `file://${dir}/hicolor/16x16/status/${file}`] : [name];
                        }
                        property int attempt: 0
                        source: sources[Math.min(attempt, sources.length - 1)]
                        // deferred: bumping the index inside the status change is a binding loop; the index sticks across icon changes
                        onStatusChanged: if (status === Image.Error && attempt < sources.length - 1) Qt.callLater(() => trayIcon.attempt++)
                        implicitSize: 16

                        TrayMenu {
                            id: menu
                            rootMenu: trayIcon.modelData.menu
                            anchor.item: trayIcon
                        }

                        MouseArea {
                            anchors.fill: parent
                            acceptedButtons: Qt.LeftButton | Qt.RightButton | Qt.MiddleButton
                            onClicked: mouse => {
                                const item = trayIcon.modelData;
                                if (mouse.button === Qt.MiddleButton)
                                    item.secondaryActivate();
                                else if (mouse.button === Qt.RightButton || item.onlyMenu)
                                    item.hasMenu && menu.toggle();
                                else
                                    item.activate();
                            }
                        }
                    }
                }
            }
        }
    }
}
