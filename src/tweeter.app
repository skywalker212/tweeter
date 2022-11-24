{application, tweeter,
    [
        {description, "A simple twitter clone"},
        {vsn, "0.1.0"},
        {modules, [ tweeter, t_sup, t_app, t_server, t_store ]},
        {registered, [t_sup]},
        {applications, [kernel, stdlib]},
        {mod, {t_app, []}}
    ]
}.