daemon = {}
config = {}

daemon.dummy = 0
daemon.busy_wait = 1

config.get_integer = _internal_get_integer
config.get_text    = _internal_get_text

throw_exception = _internal_throw_exception

function daemon.register(typ, options)
    if     typ == daemon.dummy     then
        _internal_register_daemon(typ)
    elseif typ == daemon.busy_wait then
        if options.condition and options.action then
            _internal_register_daemon(typ, options.condition, options.action)
        else
            throw_exception("condition/action must be non nil")
        end
    else
        throw_exception("invalid daemon type")
    end
    return
end
