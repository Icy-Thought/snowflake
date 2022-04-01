local function prequire(...)
    local status, lib = pcall(require, ...)
    if (status) then return lib end
    return nil
end

local colorizer = prequire("colorizer")

colorizer.setup({ "*" }, {
    RGB      = true;
    RRGGBB   = true;
    names    = true;
    RRGGBBAA = true;
    rgb_fn   = true;
    hsl_fn   = true;
    css      = true;
    css_fn   = true;
    mode = "background";
})
