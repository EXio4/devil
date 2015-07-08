-- example "daemon" (that does nothing)

function cond()
    print("[lua_example] inside-condition")
    return true
end

function action()
    print("[lua_example] inside-event")
    return
end
