-- example "daemon" (that does nothing)

daemon.register(daemon.busy_wait, {
    condition = function ()
        print("condition")
        return true
    end,
    action = function ()
        print("event")
    end
})
