{application, tweeter_client_simulator,
    [
        {description, "Client simulator for tweeter"},
        {vsn, "0.1.0"},
        {modules, [tcs_app, tcs_sup, tweeter_client]},
        {registered, [tweeter_client_simulator]},
        {applications, [kernel, stdlib]},
        {mod, {tcs_app, [5]}}
    ]
}