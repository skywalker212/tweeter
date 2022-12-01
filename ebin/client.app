{application, client,
    [
        {description, "Client simulator for tweeter"},
        {vsn, "0.1.0"},
        {modules, [c_app, c_sup, client]},
        {registered, [client]},
        {applications, [kernel, stdlib]},
        {mod, {c_app, [1000]}}
    ]
}.