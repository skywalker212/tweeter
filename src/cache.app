{application, cache,
    [
        {description, "A simple caching system"},
        {vsn, "0.1.0"},
        {modules, [ cache, c_element, c_store, c_app, c_sup ]},
        {registered, [c_sup]},
        {applications, [kernel, stdlib]},
        {mod, {c_app, []}}
    ]
}.