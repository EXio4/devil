-- example "daemon" (that just loops)

daemon.register(daemon.busy_wait, {
    condition = function ()
      return true
    end,
    action = function ()
    
    end
})
