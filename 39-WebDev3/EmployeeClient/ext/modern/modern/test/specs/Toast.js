/* global Ext, expect */

topSuite("Ext.Toast", ["Ext.Toast"], function() {
    describe("Ext.Toast", function () {
        it("should only queue up maxQueue toasts", function () {
            Ext.toast('one')
            Ext.toast('two');
            Ext.toast('three');
            Ext.toast('four');
            Ext.toast('five');
            Ext.toast('six');
            Ext.toast('seven');
            Ext.toast('eight');
            Ext.toast('nine');
            Ext.toast('ten');
            expect(Ext.Toast.prototype.getQueueCount()).toBe(Ext.Toast.prototype.config.maxQueue);
            Ext.Toast.destroy();
        });
    });
});

