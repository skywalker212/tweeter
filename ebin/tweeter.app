{application, tweeter,
    [
        {description, "A simple twitter clone"},
        {vsn, "0.1.0"},
        {modules, [ tweeter, t_app, t_worker, t_store, util ]},
        {registered, [tweeter]},
        {applications, [kernel, stdlib]},
        {mod, {t_app, []}}
    ]
}.