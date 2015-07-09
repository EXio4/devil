daemon = {}
config = {}

daemon.dummy = 0
daemon.busy_wait = 1

function daemon.register(typ, options)
    if typ == daemon.dummy then
        _internal_register_daemon(typ)
    else if typ == daemon.busy_wait then
        _internal_register_daemon(typ, options.condition, options.action)
    else
        throw_exception("invalid daemon type")
    end
end

config.get_integer = _internal_get_integer
config.get_text    = _internal_get_text

function throw_exception(text)
    return _internal_throw(text)
end