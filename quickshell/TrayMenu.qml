import QtQuick
import Quickshell
import Quickshell.Hyprland
import Quickshell.Widgets

// tray item menu drawn in QML so it follows the wal colors; submenus drill down in place
PopupWindow {
    id: root

    property var rootMenu
    property var stack: []

    function toggle() {
        stack = [];
        visible = !visible;
    }

    anchor.edges: Edges.Bottom
    anchor.gravity: Edges.Bottom
    implicitWidth: 280
    implicitHeight: column.implicitHeight + 12
    color: "transparent"

    QsMenuOpener {
        id: opener
        menu: root.stack.length ? root.stack[root.stack.length - 1] : root.rootMenu
    }

    HyprlandFocusGrab {
        windows: [root]
        active: root.backingWindowVisible // not `visible`: the grab silently fails if the surface isn't mapped yet
        onCleared: root.visible = false
    }

    component Entry: Rectangle {
        id: entry
        property string label
        property string icon
        property string trailing
        signal activated

        width: column.width
        height: 28
        radius: 6
        color: enabled && mouse.containsMouse ? Wal.color14 : "transparent"
        opacity: enabled ? 1 : 0.5

        Row {
            anchors.verticalCenter: parent.verticalCenter
            x: 8
            spacing: 8

            IconImage {
                visible: entry.icon !== ""
                implicitSize: 16
                source: entry.icon
            }

            Text {
                text: entry.label
                width: Math.min(implicitWidth, entry.width - 56)
                elide: Text.ElideRight
                color: Wal.color0
                font.family: Wal.font
                font.pixelSize: 13
            }
        }

        Text {
            anchors.right: parent.right
            anchors.rightMargin: 8
            anchors.verticalCenter: parent.verticalCenter
            text: entry.trailing
            color: Wal.color0
            font.family: Wal.font
            font.pixelSize: 13
        }

        MouseArea {
            id: mouse
            anchors.fill: parent
            hoverEnabled: true
            onClicked: entry.activated()
        }
    }

    Rectangle {
        anchors.fill: parent
        radius: 8
        color: Wal.color10
        border {
            width: 2
            color: Wal.color0
        }

        Column {
            id: column
            x: 6
            y: 6
            width: parent.width - 12

            Entry {
                visible: root.stack.length > 0
                label: "‹ Back"
                onActivated: root.stack = root.stack.slice(0, -1)
            }

            Repeater {
                model: opener.children

                Item {
                    id: item
                    required property QsMenuEntry modelData

                    width: column.width
                    height: modelData.isSeparator ? 9 : 28

                    Rectangle {
                        visible: item.modelData.isSeparator
                        anchors.centerIn: parent
                        width: parent.width - 8
                        height: 1
                        color: Wal.color0
                        opacity: 0.4
                    }

                    Entry {
                        visible: !item.modelData.isSeparator
                        label: item.modelData.text.replace(/_(?=\S)/, "") // drop the mnemonic marker
                        icon: item.modelData.icon
                        enabled: item.modelData.enabled
                        trailing: item.modelData.hasChildren ? "›"
                            : item.modelData.buttonType === QsMenuButtonType.None ? ""
                            : item.modelData.checkState === Qt.Checked ? "●" : "○"
                        onActivated: {
                            if (item.modelData.hasChildren) {
                                root.stack = [...root.stack, item.modelData];
                            } else {
                                item.modelData.triggered();
                                root.visible = false;
                            }
                        }
                    }
                }
            }
        }
    }
}
