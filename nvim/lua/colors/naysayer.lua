-- Naysayer colors
local colors = {
  background   = "#062329",
  text         = "#d1b897",
  selection    = "#0000ff",
  white        = "#ffffff",
  gutter_fg    = "#062329",
  line_fg      = "#126367",
  highlight    = "#0b3335",

  -- Monokai-derived
  yellow   = "#E6DB74",
  orange   = "#FD971F",
  red      = "#F92672",
  magenta  = "#FD5FF0",
  blue     = "#66D9EF",
  green    = "#A6E22E",
  cyan     = "#A1EFE4",
  violet   = "#AE81FF",

  constants = "#7ad0c6",
  variables = "#c1d1e3",
  functions = "#ffffff",
  strings   = "#2ec09c",
  comments  = "#44b340",
  macros    = "#8cde94",
  numbers   = "#7ad0c6",

  error     = "#ff0000",
  warning   = "#ffaa00",
}

-- Helper
local function hl(group, opts)
  vim.api.nvim_set_hl(0, group, opts)
end

-- Core
hl("Normal",         { fg = colors.text, bg = colors.background })
hl("Cursor",         { bg = colors.white })
hl("Visual",         { bg = colors.selection })
hl("LineNr",         { fg = colors.line_fg, bg = colors.background })
hl("CursorLineNr",   { fg = colors.white, bg = colors.background })
hl("CursorLine",     { bg = colors.highlight })
hl("Search",         { bg = colors.selection })
hl("IncSearch",      { bg = colors.selection })
hl("VertSplit",      { fg = colors.gutter_fg })
hl("MatchParen",     { bg = colors.selection })
hl("ColorColumn",    { bg = colors.highlight })
hl("SignColumn",     { bg = colors.background })
hl("Whitespace",     { fg = colors.warning, reverse = true })

-- Syntax
hl("Comment",              { fg = colors.comments, italic = true })
hl("Constant",             { fg = colors.constants })
hl("String",               { fg = colors.strings })
hl("Character",            { fg = colors.strings })
hl("Number",               { fg = colors.numbers })
hl("Boolean",              { fg = colors.constants })
hl("Float",                { fg = colors.numbers })
hl("Identifier",           { fg = colors.variables })
hl("Function",             { fg = colors.functions })
hl("Statement",            { fg = colors.white })
hl("Conditional",          { fg = colors.white })
hl("Repeat",               { fg = colors.white })
hl("Label",                { fg = colors.white })
hl("Operator",             { fg = colors.white })
hl("Keyword",              { fg = colors.white })
hl("Exception",            { fg = colors.white })
hl("PreProc",              { fg = colors.macros })
hl("Include",              { fg = colors.macros })
hl("Define",               { fg = colors.macros })
hl("Macro",                { fg = colors.macros })
hl("Type",                 { fg = colors.macros })
hl("StorageClass",         { fg = colors.white })
hl("Structure",            { fg = colors.white })
hl("Typedef",              { fg = colors.white })

-- Diagnostic
hl("DiagnosticError",      { fg = colors.error })
hl("DiagnosticWarn",       { fg = colors.warning })
hl("DiagnosticInfo",       { fg = colors.blue })
hl("DiagnosticHint",       { fg = colors.cyan })

-- Plugin support
hl("LineNrAbove",          { fg = colors.line_fg })
hl("LineNrBelow",          { fg = colors.line_fg })
hl("DiagnosticUnderlineError", { sp = colors.error, undercurl = true })
hl("DiagnosticUnderlineWarn",  { sp = colors.warning, undercurl = true })

-- Treesitter-specific
hl("@function.call",       { fg = colors.functions })
hl("@parameter",           { fg = colors.text })
hl("@field",               { fg = colors.text })
hl("@keyword",             { fg = colors.white })
hl("@constant.builtin",    { fg = colors.constants })

-- Rainbow delimiters (use your plugin's highlight names)
hl("RainbowDelimiterRed",     { fg = colors.red })
hl("RainbowDelimiterYellow",  { fg = colors.yellow })
hl("RainbowDelimiterBlue",    { fg = colors.blue })
hl("RainbowDelimiterGreen",   { fg = colors.green })
hl("RainbowDelimiterOrange",  { fg = colors.orange })
hl("RainbowDelimiterViolet",  { fg = colors.violet })

-- Tab bar (if using tabline)
hl("TabLine",              { fg = colors.text, bg = colors.background })
hl("TabLineSel",           { fg = colors.background, bg = colors.text })
hl("TabLineFill",          { fg = colors.text, bg = colors.background })

-- Statusline (for lualine or custom statuslines)
hl("StatusLine",           { fg = colors.background, bg = colors.text })
hl("StatusLineNC",         { fg = colors.text, bg = colors.background })

-- Compilation/quickfix-style messages
hl("QuickFixLine",         { fg = colors.background, bg = colors.green, bold = true })
hl("ErrorMsg",             { fg = colors.error })
hl("WarningMsg",           { fg = colors.warning })
