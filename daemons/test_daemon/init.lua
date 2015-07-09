-- example "daemon" (that does nothing)

throw_exception("wot")

daemon.register(daemon.dummy, {
    condition = function ()
        print("condition")
        return true
    end,
    action = function ()
        print("event")
    end
})