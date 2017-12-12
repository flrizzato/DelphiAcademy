/**
 * @since 6.5.0
 */
Ext.define('Ext.dataview.pullrefresh.Spinner', {
    extend: 'Ext.dataview.pullrefresh.Item',
    xtype: 'pullrefreshspinner',

    hideAnimation: {
        type: 'popOut'
    },

    baseCls: Ext.baseCSSPrefix + 'pullrefreshspinner',

    template: [{
        cls: Ext.baseCSSPrefix + 'pullrefreshspinner-loading-wrap',

        children: [{
            cls: Ext.baseCSSPrefix + 'pullrefreshspinner-main ' + Ext.baseCSSPrefix + 'shadow',
            reference: 'bodyEl',

            children: [{
                cls: Ext.baseCSSPrefix + 'pullrefreshspinner-loader-wrapper',

                children: [{
                    cls: Ext.baseCSSPrefix + 'pullrefreshspinner-arrow-wrapper',

                    children: [{
                        cls: Ext.baseCSSPrefix + 'pullrefreshspinner-arrow-main',
                        reference: 'arrowEl'
                    }]
                }, {
                    cls: Ext.baseCSSPrefix + 'pullrefreshspinner-spinner-wrapper',
                    reference: 'spinnerEl',

                    children: [{
                        cls: Ext.baseCSSPrefix + 'pullrefreshspinner-spinner-main',

                        children: [{
                            cls: Ext.baseCSSPrefix + 'pullrefreshspinner-spinner-left',

                            children: [{
                                cls: Ext.baseCSSPrefix + 'pullrefreshspinner-half-circle'
                            }]
                        }, {
                            cls: Ext.baseCSSPrefix + 'pullrefreshspinner-spinner-right',

                            children: [{
                                cls: Ext.baseCSSPrefix + 'pullrefreshspinner-half-circle'
                            }]
                        }]
                    }]
                }]
            }]
        }]
    }],

    privates: {
        toggleDisplay: function(load) {
            var me = this;

            me.arrowEl.setVisible(!load);
            me.spinnerEl.setVisible(load);
        },

        updatePull: function(pull) {
            var me = this,
                rotation = Math.floor(Math.min(pull, 3) * 100) - 110;

            me.bodyEl.setOpacity(Math.min(1, pull));
            me.arrowEl.dom.style.transform = 'rotate(' + rotation + 'deg)';
        },

        updateState: function(state) {
            var me = this;

            me.toggleDisplay(me.isLoading(state));
        }
    }
});
