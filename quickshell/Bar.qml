import QtQuick
import QtQuick.Layouts
import Quickshell
import Quickshell.Hyprland
import Quickshell.Io
import Quickshell.Networking
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
    implicitHeight: 32
    color: "transparent"

    Rectangle {
        anchors.fill: parent
        radius: 2
        color: Wal.surface
        border.width: 1
        border.color: Wal.outline
    }

    component Divider: Rectangle {
        implicitWidth: 1
        implicitHeight: 14
        color: Wal.outline
    }

    component Pill: Rectangle {
        property alias text: label.text
        property alias label: label
        property int maxTextWidth: 100000

        Layout.fillHeight: true
        implicitWidth: label.width + 14
        radius: 0
        color: "transparent"

        Text {
            id: label
            anchors.centerIn: parent
            width: Math.min(implicitWidth, parent.maxTextWidth)
            elide: Text.ElideRight
            color: Wal.textColor
            font {
                family: Wal.font
                pixelSize: 12
            }
        }
    }

    RowLayout {
        id: left
        anchors {
            left: parent.left
            leftMargin: 6
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
                        width: Math.max(ws.implicitWidth + 16, 32)
                        height: parent.height
                        color: wsMouse.containsMouse ? Wal.selection : "transparent"

                        Rectangle {
                            anchors.bottom: parent.bottom
                            anchors.horizontalCenter: parent.horizontalCenter
                            width: 14
                            height: 2
                            visible: parent.modelData.focused
                            color: Wal.accent
                        }

                        Text {
                            id: ws
                            anchors.centerIn: parent
                            text: parent.modelData.name.padStart(2, "0")
                            color: parent.modelData.focused ? Wal.accent : Wal.muted
                            font.family: Wal.font
                            font.pixelSize: 12
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

    }

    Pill {
        id: title
        property string eventText: ""
        property bool eventWarning: false

        function announce(message, warning = false) {
            eventText = message;
            eventWarning = warning;
            eventTimeout.restart();
        }

        // Hyprland (lua config) only reports the active window after the first focus change
        property string initial: ""

        anchors {
            horizontalCenter: parent.horizontalCenter
            top: parent.top
            bottom: parent.bottom
        }
        visible: text !== ""
        text: eventText || (Hyprland.activeToplevel?.title ?? initial)
        maxTextWidth: Math.max(0, Math.min(bar.width / 3, bar.width - 2 * Math.max(left.implicitWidth, right.implicitWidth) - 32))
        label.color: eventText ? (eventWarning ? Wal.accent : Wal.textColor) : Wal.muted

        Timer {
            id: eventTimeout
            interval: 3000
            onTriggered: title.eventText = ""
        }

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
        id: right
        anchors {
            right: parent.right
            rightMargin: 8
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

            Connections {
                target: volume.sink?.ready ? volume.sink.audio : null
                function onVolumeChanged() {
                    const audio = volume.sink.audio;
                    if (!audio.muted)
                        title.announce(`AUDIO / OUTPUT ${Math.round(audio.volume * 100)}%`);
                }
                function onMutedChanged() {
                    const muted = volume.sink.audio.muted;
                    title.announce(muted ? "AUDIO / OUTPUT MUTED" : "AUDIO / OUTPUT UNMUTED", muted);
                }
            }

            Connections {
                target: volume.source?.ready ? volume.source.audio : null
                function onMutedChanged() {
                    const muted = volume.source.audio.muted;
                    title.announce(muted ? "AUDIO / MICROPHONE MUTED" : "AUDIO / MICROPHONE UNMUTED", muted);
                }
            }

            text: {
                const out = sink?.audio?.muted ? "VOL OFF" : `VOL ${String(vol).padStart(3, " ")}%`;
                const src = source?.audio?.muted ? "MIC OFF" : `MIC ${String(Math.round((source?.audio?.volume ?? 0) * 100)).padStart(3, " ")}%`;
                return `${out}  ${src}`;
            }
            color: volumeMouse.containsMouse ? Wal.selection : "transparent"

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

        Divider {}

        Pill {
            readonly property bool online: Networking.devices.values.some(d => d.connected)
            property bool initialized: false
            Component.onCompleted: initialized = true
            onOnlineChanged: {
                if (initialized)
                    title.announce(online ? "NETWORK / CONNECTED" : "NETWORK / DISCONNECTED", !online);
            }
            text: online ? "NET UP" : "NET DOWN"
            label.color: online ? Wal.muted : Wal.accent
        }

        Pill {
            id: stats
            text: "SYS"
            label.color: monitor.visible ? Wal.accent : Wal.muted
            color: statsMouse.containsMouse || monitor.visible ? Wal.selection : "transparent"
            Accessible.role: Accessible.Button
            Accessible.name: "System usage history"
            Accessible.onPressAction: monitor.visible = !monitor.visible

            MouseArea {
                id: statsMouse
                anchors.fill: parent
                hoverEnabled: true
                cursorShape: Qt.PointingHandCursor
                onClicked: monitor.visible = !monitor.visible
            }

            SystemMonitor {
                id: monitor
                anchor.item: stats
                anchor.rect {
                    y: 8
                    width: stats.width
                    height: stats.height
                }
            }
        }

        Divider {}

        Pill {
            id: language
            property string layoutName: "…"
            text: "KEY " + (layoutName === "English (US)" ? "US" : layoutName === "Portuguese (Brazil)" ? "BR" : layoutName)
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

        Divider {}

        Pill {
            id: clockPill
            text: Qt.formatDateTime(clock.date, "hh:mm")
            label.color: calendar.visible ? Wal.accent : Wal.textColor
            color: clockMouse.containsMouse || calendar.visible ? Wal.selection : "transparent"
            Accessible.role: Accessible.Button
            Accessible.name: "Calendar"
            Accessible.onPressAction: calendar.visible = !calendar.visible

            SystemClock {
                id: clock
                precision: SystemClock.Minutes
            }

            MouseArea {
                id: clockMouse
                anchors.fill: parent
                hoverEnabled: true
                cursorShape: Qt.PointingHandCursor
                onClicked: calendar.visible = !calendar.visible
            }

            CalendarPopup {
                id: calendar
                screen: bar.screen
                margins.top: bar.margins.top + bar.height + 8
                margins.right: bar.margins.right
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
