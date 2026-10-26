import QtQuick
import Quickshell
import Quickshell.Hyprland
import Quickshell.Io
import Quickshell.Wayland
import Quickshell.Widgets

// app launcher, toggled with `qs ipc call launcher toggle`
PanelWindow {
    id: root

    visible: false
    anchors.top: true
    margins.top: screen.height * 0.08
    implicitWidth: Math.min(560, screen.width - 32)
    implicitHeight: Math.min(460, screen.height * 0.5)
    exclusionMode: ExclusionMode.Ignore
    color: "transparent"
    WlrLayershell.layer: WlrLayer.Overlay
    WlrLayershell.keyboardFocus: WlrKeyboardFocus.Exclusive
    WlrLayershell.namespace: "launcher"

    readonly property var apps: {
        const q = input.text.toLowerCase();
        const all = DesktopEntries.applications.values.filter(a => !a.noDisplay);
        const rank = a => a.name.toLowerCase().startsWith(q) ? 0 : 1;
        return all
            .filter(a => [a.name, a.genericName, ...a.keywords].join(" ").toLowerCase().includes(q))
            .sort((a, b) => rank(a) - rank(b) || a.name.localeCompare(b.name));
    }

    function launch(app) {
        if (app.runInTerminal)
            Quickshell.execDetached(["kitty", ...app.command]);
        else
            app.execute();
        visible = false;
    }

    onVisibleChanged: {
        if (!visible)
            return;
        input.text = "";
        list.currentIndex = 0;
        input.forceActiveFocus();
    }

    IpcHandler {
        target: "launcher"
        function toggle(): void {
            root.visible = !root.visible;
        }
    }

    // click outside closes it
    HyprlandFocusGrab {
        windows: [root]
        active: root.backingWindowVisible // not `visible`: the grab silently fails if the surface isn't mapped yet
        onCleared: root.visible = false
    }

    Rectangle {
        anchors.fill: parent
        radius: 10
        color: Wal.surface
        border {
            width: 1
            color: Wal.outline
        }

        Column {
            anchors.fill: parent
            anchors.margins: 20
            spacing: 16

            TextInput {
                id: input
                width: parent.width
                color: Wal.textColor
                selectionColor: Wal.selection
                selectedTextColor: Wal.textColor
                font.family: Wal.font
                font.pixelSize: 16
                onTextChanged: list.currentIndex = 0

                Keys.onPressed: event => {
                    const ctrl = event.modifiers & Qt.ControlModifier;
                    if (event.key === Qt.Key_Escape)
                        root.visible = false;
                    else if (event.key === Qt.Key_Return || event.key === Qt.Key_Enter)
                        list.currentItem && root.launch(list.currentItem.modelData);
                    else if (event.key === Qt.Key_Down || ctrl && (event.key === Qt.Key_N || event.key === Qt.Key_J))
                        list.incrementCurrentIndex();
                    else if (event.key === Qt.Key_Up || ctrl && (event.key === Qt.Key_P || event.key === Qt.Key_K))
                        list.decrementCurrentIndex();
                    else
                        return;
                    event.accepted = true;
                }

                Text {
                    visible: !input.text
                    text: "Search applications…"
                    color: Wal.muted
                    font: input.font
                }
            }

            ListView {
                id: list
                width: parent.width
                height: parent.height - input.height - parent.spacing
                clip: true
                model: root.apps
                highlightMoveDuration: 0
                highlight: Rectangle {
                    radius: 6
                    color: Wal.selection
                }

                delegate: Item {
                    required property DesktopEntry modelData
                    required property int index
                    width: list.width
                    height: 40

                    Row {
                        anchors.verticalCenter: parent.verticalCenter
                        x: 8
                        spacing: 10

                        IconImage {
                            implicitSize: 22
                            source: Quickshell.iconPath(parent.parent.modelData.icon, true)
                        }

                        Text {
                            anchors.verticalCenter: parent.verticalCenter
                            text: parent.parent.modelData.name
                            color: Wal.textColor
                            font.family: Wal.font
                            font.pixelSize: 14
                        }
                    }

                    MouseArea {
                        anchors.fill: parent
                        hoverEnabled: true
                        onEntered: list.currentIndex = parent.index
                        onClicked: root.launch(parent.modelData)
                    }
                }
            }
        }
    }
}
