Ext.define('Ext.theme.material.layout.overflow.Scroller', {
    override: 'Ext.layout.overflow.Scroller',

    config: {
        backwardTool: {
            ripple: {
                centered: false,
                bound: true,
                diameterLimit: false
            }
        },

        forwardTool: {
            ripple: {
                centered: false,
                bound: true,
                diameterLimit: false
            }
        }
    }
});
