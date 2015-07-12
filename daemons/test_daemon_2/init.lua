-- example "daemon" (that does nothing, but crash)

local i = 0

daemon.register(daemon.busy_wait, {
    condition = function ()
        print("condition")
        i = i + 1
        return (i > 2)
    end,
    action = function ()
        print("event")
        throw_exception("...")
    end
})
