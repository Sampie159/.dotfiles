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
    implicitHeight: 34
    color: "transparent"

    component Island: Rectangle {
        height: bar.height
        radius: 10
        color: Wal.surface
        border.width: 1
        border.color: Wal.outline
    }

    component Pill: Rectangle {
        property alias text: label.text
        property alias label: label
        property int maxTextWidth: 100000

        Layout.fillHeight: true
        implicitWidth: label.width + 20
        radius: 6
        color: "transparent"

        Text {
            id: label
            anchors.centerIn: parent
            width: Math.min(implicitWidth, parent.maxTextWidth)
            elide: Text.ElideRight
            color: Wal.foreground
            font {
                family: Wal.font
                pixelSize: 13
            }
        }
    }

    Island {
        id: leftIsland
        implicitWidth: leftContents.implicitWidth + 8

        RowLayout {
            id: leftContents
            anchors.fill: parent
            anchors.margins: 4
            spacing: 4

            Pill {
                implicitWidth: workspaces.implicitWidth + 4

                Row {
                    id: workspaces
                    anchors.centerIn: parent
                    spacing: 2
                    height: parent.height

                    Repeater {
                        model: Hyprland.workspaces

                        Rectangle {
                            required property HyprlandWorkspace modelData

                            visible: modelData.id > 0 // hide special workspaces (pypr scratchpads)
                            width: Math.max(ws.implicitWidth + 14, 24)
                            height: parent.height
                            radius: 6
                            color: modelData.focused ? Wal.selection : wsMouse.containsMouse ? Wal.hover : "transparent"

                            Text {
                                id: ws
                                anchors.centerIn: parent
                                text: parent.modelData.name
                                color: parent.modelData.focused ? Wal.color14 : Wal.muted
                                font.family: Wal.font
                                font.pixelSize: 13
                            }

                            MouseArea {
                                id: wsMouse
                                anchors.fill: parent
                                hoverEnabled: true
                                cursorShape: Qt.PointingHandCursor
                                onClicked: parent.modelData.activate()
                            }
                        }
                    }
                }
            }

            Rectangle {
                visible: media.visible || title.visible
                implicitWidth: 1
                implicitHeight: 14
                color: Wal.outline
            }

            Pill {
                id: media
                readonly property MprisPlayer player: Mpris.players.values.find(p => p.identity.toLowerCase().includes("spotify")) ?? null
                readonly property string track: player ? `${player.trackArtist} - ${player.trackTitle}` : ""

                visible: player !== null
                text: "  " + track
                maxTextWidth: Math.max(0, Math.min(320, bar.width / 2 - workspaces.width - clockIsland.width / 2 - 40))
                color: mediaMouse.containsMouse ? Wal.hover : "transparent"

                MouseArea {
                    id: mediaMouse
                    anchors.fill: parent
                    hoverEnabled: true
                    cursorShape: Qt.PointingHandCursor
                    onClicked: media.player.togglePlaying()
                }
            }

            Pill {
                id: title

                // Hyprland (lua config) only reports the active window after the first focus change
                property string initial: ""

                visible: !media.visible && text !== ""
                text: Hyprland.activeToplevel?.title ?? initial
                maxTextWidth: media.maxTextWidth
                label.color: Wal.muted

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
        }
    }

    Island {
        id: clockIsland
        anchors.horizontalCenter: parent.horizontalCenter
        implicitWidth: clockLabel.implicitWidth + 8

        Pill {
            id: clockLabel
            anchors.fill: parent
            anchors.margins: 4
            property bool showDate: false
            text: Qt.formatDateTime(clock.date, showDate ? "dd-MM-yyyy" : "hh:mm")
            color: clockMouse.containsMouse ? Wal.hover : "transparent"

            SystemClock {
                id: clock
                precision: SystemClock.Minutes
            }

            MouseArea {
                id: clockMouse
                anchors.fill: parent
                hoverEnabled: true
                cursorShape: Qt.PointingHandCursor
                onClicked: clockLabel.showDate = !clockLabel.showDate
            }
        }
    }

    Island {
        anchors.right: parent.right
        implicitWidth: rightContents.implicitWidth + 8

        RowLayout {
            id: rightContents
            anchors.fill: parent
            anchors.margins: 4
            spacing: 2

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
                    return `${out}  ${src}`;
                }
                color: volumeMouse.containsMouse ? Wal.hover : "transparent"

                MouseArea {
                    id: volumeMouse
                    anchors.fill: parent
                    hoverEnabled: true
                    cursorShape: Qt.PointingHandCursor
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
                text: online ? "" : ""
                label.color: online ? Wal.muted : Wal.color11
                Accessible.name: online ? "Network connected" : "Network disconnected"
            }

            Pill {
                id: stats
                text: ""
                label.color: statsPopup.visible ? Wal.color14 : Wal.muted
                color: statsMouse.containsMouse || statsPopup.visible ? Wal.hover : "transparent"
                Accessible.role: Accessible.Button
                Accessible.name: "System usage"
                Accessible.onPressAction: statsPopup.visible = !statsPopup.visible

                MouseArea {
                    id: statsMouse
                    anchors.fill: parent
                    hoverEnabled: true
                    cursorShape: Qt.PointingHandCursor
                    onClicked: statsPopup.visible = !statsPopup.visible
                }

                PopupWindow {
                    id: statsPopup
                    anchor.item: stats
                    anchor.edges: Edges.Bottom | Edges.Right
                    anchor.gravity: Edges.Bottom | Edges.Left
                    anchor.rect {
                        y: 12
                        width: stats.width
                        height: stats.height
                    }
                    implicitWidth: 220
                    implicitHeight: statsColumn.implicitHeight + 32
                    color: "transparent"

                    HyprlandFocusGrab {
                        windows: [statsPopup]
                        active: statsPopup.backingWindowVisible
                        onCleared: statsPopup.visible = false
                    }

                    Rectangle {
                        anchors.fill: parent
                        radius: 10
                        color: Wal.surface
                        border.width: 1
                        border.color: Wal.outline
                        focus: true
                        Keys.onEscapePressed: statsPopup.visible = false

                        Column {
                            id: statsColumn
                            anchors.fill: parent
                            anchors.margins: 16
                            spacing: 12

                            Text {
                                text: "System"
                                color: Wal.muted
                                font.family: Wal.font
                                font.pixelSize: 12
                            }

                            Text {
                                id: cpu
                                property real lastIdle: 0
                                property real lastTotal: 0
                                property int usage: 0
                                text: `CPU   ${usage}%`
                                color: Wal.foreground
                                font.family: Wal.font
                                font.pixelSize: 13

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

                            Text {
                                id: mem
                                property int usage: 0
                                text: `RAM   ${usage}%`
                                color: Wal.foreground
                                font.family: Wal.font
                                font.pixelSize: 13

                                FileView {
                                    id: meminfo
                                    path: "/proc/meminfo"
                                    onLoaded: {
                                        const kb = key => Number(text().match(new RegExp(`${key}:\\s+(\\d+)`))[1]);
                                        mem.usage = Math.round(100 * (1 - kb("MemAvailable") / kb("MemTotal")));
                                    }
                                }
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
                }
            }

            Pill {
                id: language
                property string layoutName: "…"
                text: layoutName === "English (US)" ? "US" : layoutName === "Portuguese (Brazil)" ? "BR" : layoutName
                label.color: Wal.muted
                Accessible.name: layoutName

                Connections {
                    target: Hyprland
                    function onRawEvent(event) {
                        if (event.name === "activelayout")
                            language.layoutName = event.data.split(",").pop();
                    }
                }

                Process {
                    running: true
                    command: ["hyprctl", "devices", "-j"]
                    stdout: StdioCollector {
                        onStreamFinished: {
                            const kbd = JSON.parse(text).keyboards.find(k => k.main);
                            if (kbd)
                                language.layoutName = kbd.active_keymap;
                        }
                    }
                }
            }

            Pill {
                visible: SystemTray.items.values.length > 0
                implicitWidth: tray.implicitWidth + 20

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
                                cursorShape: Qt.PointingHandCursor
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
}
