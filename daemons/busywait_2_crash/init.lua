-- example "busy wait" daemon that's meant to crash
-- showing how "every" daemon is stand-alone

local i = 0

daemon.register(daemon.busy_wait, {
    condition = function ()
        i = i + 1
        return (i > 2)
    end,
    action = function ()
        print("event")
        throw_exception("...")
    end
})
