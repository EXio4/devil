daemon = {}
config = {}
chan = {}

daemon.dummy     = 0
daemon.busy_wait = 1
daemon.loop      = 2

config.get_integer = _internal_get_integer
config.get_text    = _internal_get_text

throw_exception = function (err)
    return error("'LUA_EXC_" .. err)
end

print = _internal_log_info
sleep = _internal_sleep

function daemon.register(typ, options)
    if     typ == daemon.dummy     then
        _internal_register_daemon(typ)
    elseif typ == daemon.busy_wait then
        if options and options.condition and options.action then
            _internal_register_daemon(typ, options)
        else
            throw_exception("condition/action must be non nil")
        end
    elseif typ == daemon.loop then
        if options and options.loop then
            _internal_register_daemon(typ,options)
        else
          throw_exception("loop must be non nil")
        end
    else
        throw_exception("invalid daemon type")
    end
    return
end

function chan.read(name)
  return _internal_read_chan(name)
end
function chan.write(name, value)
  return _internal_send_chan(name, value)
end
