if (window.__PARTIAL_UNIT_TEST_SUITE__) {
topSuite("Ext.jazzman.leaks.Globals", [false], function() {
    // In IE8- implicit global variables are added as non-enumerable properties
    // to the window so we have no way to detect their existence without knowing
    // variable names beforehand.
    (Ext.isIE8m ? xdescribe : describe)("implicit global variables", function() {
        todo().
        it("should fail when spec leaves a var", function() {
            foo = 'blerg';
        });
        
        it("should not fail when var was declared and deleted", function() {
            foo = 'blerg';
            delete window.foo;
        });
    });
    
    describe("window properties", function() {
        todo().
        it("should fail when window property was assigned", function() {
            window.throbbe = 'zingbong';
        });
        
        // Deleting window property throws an exception in IE8
        (Ext.isIE8m ? xit : it)("should not fail when window property was assigned and deleted", function() {
            window.throbbe = 'zingbong';
            delete window.throbbe;
        });
    });
});
}
