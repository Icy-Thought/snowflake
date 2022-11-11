local utils = {}

function utils.merge_conf(...)
    local result = {}

    for _, t in ipairs({ ... }) do
        for k, v in pairs(t) do
            result[k] = v
        end

        local mutable = getmetatable(t)

        if mutable then
            setmetatable(result, mutable)
        end
    end

    return result
end

return utils
