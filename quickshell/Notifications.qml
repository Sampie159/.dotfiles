import QtQuick
import Quickshell
import Quickshell.Services.Notifications
import Quickshell.Wayland
import Quickshell.Widgets

Scope {
    NotificationServer {
        id: server
        actionsSupported: true
        bodyMarkupSupported: true
        imageSupported: true
        onNotification: n => n.tracked = true
    }

    PanelWindow {
        visible: server.trackedNotifications.values.length > 0
        anchors {
            top: true
            right: true
        }
        margins {
            top: 8
            right: 8
        }
        implicitWidth: 500
        implicitHeight: list.contentHeight
        exclusiveZone: 0
        color: "transparent"
        WlrLayershell.layer: WlrLayer.Overlay
        WlrLayershell.namespace: "notifications"

        ListView {
            id: list
            anchors.fill: parent
            interactive: false
            spacing: 8
            verticalLayoutDirection: ListView.BottomToTop // newest on top
            model: server.trackedNotifications

            delegate: Rectangle {
                id: card
                required property Notification modelData

                width: list.width
                height: content.implicitHeight + 20
                radius: 8
                color: Qt.rgba(Wal.background.r, Wal.background.g, Wal.background.b, 0x89 / 255)
                border {
                    width: 2
                    color: Wal.color11
                }

                Timer {
                    running: card.modelData.urgency !== NotificationUrgency.Critical && !mouse.containsMouse
                    interval: card.modelData.expireTimeout > 0 ? card.modelData.expireTimeout * 1000 : 5000
                    onTriggered: card.modelData.expire()
                }

                MouseArea {
                    id: mouse
                    anchors.fill: parent
                    hoverEnabled: true
                    acceptedButtons: Qt.LeftButton | Qt.RightButton
                    onClicked: event => {
                        const n = card.modelData;
                        const def = n.actions.find(a => a.identifier === "default");
                        if (event.button === Qt.LeftButton && def)
                            def.invoke();
                        else
                            n.dismiss();
                    }
                }

                Row {
                    id: content
                    x: 10
                    y: 10
                    width: parent.width - 20
                    spacing: 10

                    IconImage {
                        id: icon
                        readonly property string src: card.modelData.image || card.modelData.appIcon
                        visible: src !== ""
                        implicitSize: 48
                        source: src.includes("/") ? src : Quickshell.iconPath(src, true)
                    }

                    Column {
                        width: parent.width - (icon.visible ? icon.width + parent.spacing : 0)
                        spacing: 4

                        Text {
                            width: parent.width
                            text: card.modelData.summary
                            elide: Text.ElideRight
                            color: Wal.foreground
                            font.family: Wal.font
                            font.pixelSize: 13
                            font.bold: true
                        }

                        Text {
                            width: parent.width
                            visible: text !== ""
                            text: card.modelData.body
                            textFormat: Text.StyledText
                            wrapMode: Text.Wrap
                            maximumLineCount: 6
                            elide: Text.ElideRight
                            color: Wal.foreground
                            font.family: Wal.font
                            font.pixelSize: 13
                        }

                        Row {
                            spacing: 6

                            Repeater {
                                model: card.modelData.actions.filter(a => a.identifier !== "default")

                                Rectangle {
                                    required property NotificationAction modelData
                                    width: actionText.implicitWidth + 16
                                    height: actionText.implicitHeight + 8
                                    radius: 6
                                    color: Wal.color11

                                    Text {
                                        id: actionText
                                        anchors.centerIn: parent
                                        text: parent.modelData.text
                                        color: Wal.background
                                        font.family: Wal.font
                                        font.pixelSize: 12
                                    }

                                    MouseArea {
                                        anchors.fill: parent
                                        onClicked: parent.modelData.invoke()
                                    }
                                }
                            }
                        }
                    }
                }
            }
        }
    }
}
