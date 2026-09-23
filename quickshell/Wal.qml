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
    readonly property string font: "CaskaydiaMono Nerd Font Propo"

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
