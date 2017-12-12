/*
 * This file launches the application by asking Ext JS to create
 * and launch() the Application class.
 */
Ext.application({
    extend: 'EmployeeClient.Application',

    name: 'EmployeeClient',

    requires: [
        // This will automatically load all classes in the EmployeeClient namespace
        // so that application classes do not need to require each other.
        'EmployeeClient.*'
    ],

    // The name of the initial view to create.
    mainView: 'EmployeeClient.view.main.Main'
});
