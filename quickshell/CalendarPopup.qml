import QtQuick
import QtQuick.Controls.Basic
import QtQuick.Layouts
import Quickshell
import Quickshell.Hyprland
import Quickshell.Wayland

PanelWindow {
    id: root

    visible: false
    property date displayedMonth: new Date()

    function browse(offset) {
        displayedMonth = new Date(displayedMonth.getFullYear(), displayedMonth.getMonth() + offset, 1);
    }

    function showToday() {
        displayedMonth = new Date();
    }

    onVisibleChanged: if (visible) showToday()
    anchors.top: true
    exclusionMode: ExclusionMode.Ignore
    WlrLayershell.layer: WlrLayer.Overlay
    WlrLayershell.keyboardFocus: WlrKeyboardFocus.Exclusive
    WlrLayershell.namespace: "calendar"
    implicitWidth: 320
    implicitHeight: column.implicitHeight + 32
    color: "transparent"

    HyprlandFocusGrab {
        windows: [root]
        active: root.backingWindowVisible
        onCleared: root.visible = false
    }

    component NavButton: Button {
        implicitWidth: 32
        implicitHeight: 28
        font.family: Wal.font
        font.pixelSize: 13
        palette.buttonText: Wal.textColor

        background: Rectangle {
            radius: 6
            color: parent.hovered || parent.down ? Wal.selection : "transparent"
            border.width: 1
            border.color: parent.activeFocus ? Wal.accent : Wal.outline
        }
    }

    Rectangle {
        anchors.fill: parent
        radius: 10
        color: Wal.surface
        border.width: 1
        border.color: Wal.outline
        focus: true
        Keys.onEscapePressed: root.visible = false
        Keys.onPressed: event => {
            if (event.key === Qt.Key_Left || event.key === Qt.Key_PageUp)
                root.browse(-1);
            else if (event.key === Qt.Key_Right || event.key === Qt.Key_PageDown)
                root.browse(1);
            else if (event.key === Qt.Key_Home)
                root.showToday();
            else
                return;
            event.accepted = true;
        }

        Column {
            id: column
            anchors.fill: parent
            anchors.margins: 16
            spacing: 12

            RowLayout {
                width: parent.width
                spacing: 8

                NavButton {
                    text: "‹"
                    Accessible.name: "Previous month"
                    onClicked: root.browse(-1)
                }

                Text {
                    Layout.fillWidth: true
                    text: grid.title
                    horizontalAlignment: Text.AlignHCenter
                    color: Wal.textColor
                    font.family: Wal.font
                    font.pixelSize: 14
                }

                NavButton {
                    text: "›"
                    Accessible.name: "Next month"
                    onClicked: root.browse(1)
                }
            }

            DayOfWeekRow {
                width: parent.width
                height: 20
                padding: 0
                spacing: grid.spacing
                locale: grid.locale

                delegate: Text {
                    required property string shortName
                    text: shortName
                    horizontalAlignment: Text.AlignHCenter
                    verticalAlignment: Text.AlignVCenter
                    color: Wal.muted
                    font.family: Wal.font
                    font.pixelSize: 11
                }
            }

            MonthGrid {
                id: grid
                width: parent.width
                height: 204
                padding: 0
                spacing: 2
                month: root.displayedMonth.getMonth()
                year: root.displayedMonth.getFullYear()

                delegate: Text {
                    required property var model
                    text: model.day
                    horizontalAlignment: Text.AlignHCenter
                    verticalAlignment: Text.AlignVCenter
                    color: model.today ? Wal.accent : model.month === grid.month ? Wal.textColor : Wal.muted
                    font.family: Wal.font
                    font.pixelSize: 12
                    Accessible.role: Accessible.StaticText
                    Accessible.name: Qt.formatDateTime(model.date, "dddd, d MMMM yyyy")

                    Rectangle {
                        anchors.fill: parent
                        z: -1
                        radius: 6
                        color: parent.model.today ? Wal.selection : "transparent"
                        border.width: parent.model.today ? 1 : 0
                        border.color: Wal.accent
                    }
                }
            }

            NavButton {
                width: parent.width
                text: "Today"
                onClicked: root.showToday()
            }
        }
    }
}
