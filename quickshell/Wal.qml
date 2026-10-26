pragma Singleton
import QtQuick
import Quickshell
import Quickshell.Io

// pywal colors, re-read whenever wal rewrites the file
Singleton {
    id: root

    property var data: ({})
    readonly property var colors: data.colors ?? {}
    readonly property color background: data.special?.background ?? "#0d0606"
    readonly property color foreground: data.special?.foreground ?? "#c2c0c0"
    readonly property color color0: colors.color0 ?? "#0d0606"
    readonly property color color10: colors.color10 ?? "#9d9ba6"
    readonly property color color11: colors.color11 ?? "#c4b6bf"
    readonly property color color14: colors.color14 ?? "#e6e4f4"
    readonly property color surface: Qt.alpha("#181a1d", 0.97)
    readonly property color textColor: "#d4d7dc"
    readonly property color outline: Qt.alpha(textColor, 0.16)
    readonly property color muted: Qt.alpha(textColor, 0.8)
    readonly property color hover: Qt.alpha(textColor, 0.08)
    readonly property color accent: colors.color14 ?? "#ddb274"
    readonly property color selection: Qt.alpha(accent, 0.16)
    readonly property string font: "Monaspace Neon NF"

    FileView {
        path: Quickshell.env("HOME") + "/.cache/wal/colors.json"
        watchChanges: true
        onFileChanged: reload()
        onLoaded: {
            try {
                root.data = JSON.parse(text());
            } catch (e) {} // wal mid-write, keep the old colors
        }
    }
}
