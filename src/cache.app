{application, cache,
    [
        {description, "A simple caching system"},
        {vsn, "0.1.0"},
        {modules, [ c_app, c_sup ]},
        {registered, [c_sup]},
        {applications, [kernel, stdlib]},
        {mod, {c_app, []}}
    ]
}.