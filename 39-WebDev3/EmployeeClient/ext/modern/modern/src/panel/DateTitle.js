/**
 * @private
 */
Ext.define('Ext.panel.DateTitle', {
    extend: 'Ext.panel.Title',
    xtype: 'datetitle',

    classCls: Ext.baseCSSPrefix + 'datetitle',

    template: [{
        reference: 'bodyElement',
        cls: Ext.baseCSSPrefix + 'body-el',

        children: [{
            reference: 'iconElement',
            cls: Ext.baseCSSPrefix + 'icon-el ' + Ext.baseCSSPrefix + 'font-icon'
        }, {
            cls: Ext.baseCSSPrefix + 'text-container-el',
            children: [{
                reference: 'yearElement',
                cls: Ext.baseCSSPrefix + 'year-el'
            }, {
                reference: 'textElement',
                cls: Ext.baseCSSPrefix + 'text-el'
            }]
        }]
    }],

    config: {
        split: {
            cached: true,
            $value: false
        },
        titleActive: {
            cached: true,
            $value: true
        },
        year: null
    },

    initialize: function() {
        var me = this;

        me.callParent();
        me.yearElement.on('tap', 'onYearTap', me);
        me.textElement.on('tap', 'onTextTap', me);
    },

    updateSplit: function(split) {
        this.yearElement.setDisplayed(split);
        this.toggleCls(Ext.baseCSSPrefix + 'split', split);
    },

    updateTitleActive: function(titleActive) {
        var cls = Ext.baseCSSPrefix + 'inactive';

        this.textElement.toggleCls(cls, !titleActive);
        this.yearElement.toggleCls(cls, titleActive);
    },

    updateYear: function(year)  {
        this.yearElement.dom.textContent = year;
    },

    privates: {
        onTextTap: function(e) {
            this.fireEvent('titletap', this, e);
        },

        onYearTap: function(e) {
            this.fireEvent('yeartap', this, e);
        }
    }
});