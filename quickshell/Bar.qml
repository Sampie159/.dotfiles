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
            color: Wal.textColor
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
                                color: parent.modelData.focused ? Wal.accent : Wal.muted
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
                visible: title.visible
                implicitWidth: 1
                implicitHeight: 14
                color: Wal.outline
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

                visible: text !== ""
                text: eventText || (Hyprland.activeToplevel?.title ?? initial)
                maxTextWidth: Math.max(0, Math.min(320, bar.width / 2 - workspaces.width - clockIsland.width / 2 - 40))
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
            text: Qt.formatDateTime(clock.date, "hh:mm")
            label.color: calendar.visible ? Wal.accent : Wal.textColor
            color: clockMouse.containsMouse || calendar.visible ? Wal.hover : "transparent"
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
                property bool initialized: false
                Component.onCompleted: initialized = true
                onOnlineChanged: {
                    if (initialized)
                        title.announce(online ? "NETWORK / CONNECTED" : "NETWORK / DISCONNECTED", !online);
                }
                text: online ? "" : ""
                label.color: online ? Wal.muted : Wal.accent
                Accessible.name: online ? "Network connected" : "Network disconnected"
            }

            Pill {
                id: stats
                text: ""
                label.color: statsPopup.visible ? Wal.accent : Wal.muted
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

                SystemMonitor {
                    id: statsPopup
                    anchor.item: stats
                    anchor.rect {
                        y: 12
                        width: stats.width
                        height: stats.height
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
