Ext.define('EmployeeClient.store.Personnel', {
   extend: 'Ext.data.Store',
   
   requires: [
       'EmployeeClient.model.Personnel',
       'Ext.data.proxy.Ajax',
       'Ext.data.reader.Json'
   ],   
   
   alias: 'store.employeestore',
   constructor: function(cfg) {
       var me = this;
       cfg = cfg || {};
       me.callParent([Ext.apply({
           autoLoad: true,
           model: 'EmployeeClient.model.Personnel',
           proxy: {
               type: 'ajax',
               url: 'http://localhost:8080/EmployeeServer',
               reader: {
                   type: 'json',
                   rootProperty: 'items'
               }
           }
       }, cfg)]);
   }
});
