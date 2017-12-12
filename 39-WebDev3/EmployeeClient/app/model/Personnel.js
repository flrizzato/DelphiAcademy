Ext.define('EmployeeClient.model.Personnel', {
   extend: 'Ext.data.Model',
   
   requires: [
       'Ext.data.field.Field'
   ],
   
   fields: [
       {
           name: 'first_name'
       },
       {
           name: 'last_name'
       },
       {
           name: 'phone_ext'
       },
       {
           name: 'dept_no'
       },
       {
           name: 'job_country'
       }
   ]
});
