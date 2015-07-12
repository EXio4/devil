-- example "loop" daemon, which reads from a chan, and prints values there

daemon.register(daemon.loop, {
    loop = function ()
          local x = chan.read("loop_example")
          print("got " .. x .. " from loop_example!")
    end
})
