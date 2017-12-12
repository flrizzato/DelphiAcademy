/**
 *
 */
Ext.define('Ext.layout.wrapper.Inner', {
    config: {
        container: null,
        manageBorders: null
    },

    constructor: function(config) {
        this.initConfig(config);
    },

    getElement: function() {
        var container = this.getContainer();

        // bodySizerElement is a workaround for workaround for the following issues:
        // https://bugs.webkit.org/show_bug.cgi?id=150445
        // https://bugs.webkit.org/show_bug.cgi?id=137730
        // See Ext.Container#autoSize
        return container.boxScrollerElement || container.bodySizerElement ||
            container.bodyElement;
    },

    setInnerWrapper: Ext.emptyFn,

    getInnerWrapper: Ext.emptyFn
});
