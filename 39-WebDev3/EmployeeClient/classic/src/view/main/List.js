/**
 * This view is an example list of people.
 */
Ext.define('EmployeeClient.view.main.List', {
    extend: 'Ext.grid.Panel',
    xtype: 'mainlist',
	
    requires: [
        'EmployeeClient.store.Personnel'
    ],

    title: 'Personnel',

    store: {type: 'employeestore'},

    columns: [
        { text: 'First Name',  dataIndex: 'first_name' },
        { text: 'Last Name', dataIndex: 'last_name', flex: 1 },
        { text: 'Phone', dataIndex: 'phone_ext', flex: 1 },
        { text: 'Depto', dataIndex: 'dept_no', flex: 1 },
        { text: 'Country', dataIndex: 'job_country', flex: 1 }
    ],

    listeners: {
        select: 'onItemSelected'
    }
});
