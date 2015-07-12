-- example "loop" daemon, using os.sleep and sending values to the other loop thread

local i = 0

daemon.register(daemon.loop, {
    loop = function ()
        chan.write("loop_example", i)
        print("sending ".. i .. " to loop_example")
        i = i + 1
        sleep(2)
    end
})
