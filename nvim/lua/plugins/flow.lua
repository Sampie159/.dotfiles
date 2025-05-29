return {
    "0xstepit/flow.nvim",
    lazy = false,
    priority = 1000,
    config = function()
        require("flow").setup{}
    end
}
