import QtQuick
import Quickshell
import Quickshell.Hyprland
import Quickshell.Io

PopupWindow {
    id: root

    anchor.edges: Edges.Bottom | Edges.Right
    anchor.gravity: Edges.Bottom | Edges.Left
    implicitWidth: 280
    implicitHeight: column.implicitHeight + 32
    color: "transparent"

    HyprlandFocusGrab {
        windows: [root]
        active: root.backingWindowVisible
        onCleared: root.visible = false
    }

    component Meter: Item {
        property string label
        property int value: 0
        property var history: []

        width: column.width
        implicitHeight: 52

        Text {
            text: parent.label
            color: Wal.muted
            font.family: Wal.font
            font.pixelSize: 12
        }

        Text {
            anchors.right: parent.right
            text: `${parent.value}%`
            color: Wal.textColor
            font.family: Wal.font
            font.pixelSize: 12
        }

        Row {
            anchors.right: parent.right
            anchors.bottom: parent.bottom
            height: 24
            spacing: 2

            Repeater {
                model: parent.parent.history

                Rectangle {
                    required property int modelData
                    anchors.bottom: parent.bottom
                    width: (column.width - 62) / 32
                    height: Math.max(1, parent.height * modelData / 100)
                    color: Wal.accent
                }
            }
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

        Column {
            id: column
            anchors.fill: parent
            anchors.margins: 16
            spacing: 16

            Text {
                text: "SYSTEM / LAST 64s"
                color: Wal.muted
                font.family: Wal.font
                font.pixelSize: 11
            }

            Meter {
                id: cpu
                label: "CPU"
                property real lastIdle: 0
                property real lastTotal: 0

                FileView {
                    id: stat
                    path: "/proc/stat"
                    onLoaded: {
                        const f = text().split("\n")[0].trim().split(/\s+/).slice(1).map(Number);
                        const idle = f[3] + f[4];
                        const total = f.reduce((a, b) => a + b, 0);
                        if (cpu.lastTotal > 0 && total > cpu.lastTotal) {
                            cpu.value = Math.round(100 * (1 - (idle - cpu.lastIdle) / (total - cpu.lastTotal)));
                            cpu.history = [...cpu.history.slice(-31), cpu.value];
                        }
                        cpu.lastIdle = idle;
                        cpu.lastTotal = total;
                    }
                }
            }

            Meter {
                id: mem
                label: "RAM"

                FileView {
                    id: meminfo
                    path: "/proc/meminfo"
                    onLoaded: {
                        const kb = key => Number(text().match(new RegExp(`${key}:\\s+(\\d+)`))[1]);
                        mem.value = Math.round(100 * (1 - kb("MemAvailable") / kb("MemTotal")));
                        mem.history = [...mem.history.slice(-31), mem.value];
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
