/**
 * A simple header component for grouped lists.  List headers are created automatically
 * by {@link Ext.dataview.List Lists} and should not be directly instantiated.
 */
Ext.define('Ext.dataview.ItemHeader', {
    extend: 'Ext.Component',
    xtype: 'itemheader',

    mixins: [
        'Ext.mixin.Toolable',
        'Ext.dataview.Pinnable'
    ],

    isItemHeader: true,

    config: {
        /**
         * @cfg {Ext.util.Group} group
         * The {@link Ext.util.Collection collection} of {@link Ext.data.Model records}
         * in the group.
         * @readonly
         * @since 6.5.0
         */
        group: null,

        contentWidth: null
    },

    /**
     * @cfg {String/String[]/Ext.XTemplate} tpl
     * This is the normal {@link Ext.Component#cfg!tpl tpl} config but in this context,
     * this {@link Ext.XTemplate template} has access to a specialized data object.
     * The data object available to the template provides the following properties:
     *
     *  * `name` The grouping string of the {@link Ext.data.Store#groupField groupField}
     *    for the group header. This string is the string produced by grouper's
     *    {@link Ext.util.Grouper#groupFn groupFn}.
     *  * `value` The value of the {@link Ext.data.Store#groupField groupField}
     *    for the group header being rendered.
     *  * `columnName` The column header associated with the field being grouped
     *    by *if there is a column for the field*, falls back to the `groupField`.
     *  * `groupField` The field name being grouped by.
     *  * `html` The rendering of the `value` as handled by the cell (for a grid,
     *    otherwise the same as `name`).
     *  * `children` An array containing the child records for the group. **This is not
     *    available if the store is a {@link Ext.data.BufferedStore BufferedStore}.**
     *
     * @since 6.5.0
     */

    html: '\xA0',

    classCls: Ext.baseCSSPrefix + 'itemheader',

    inheritUi: true,

    toolDefaults: {
        ui: 'itemheader'
    },

    template: [{
        reference: 'bodyElement',
        cls: Ext.baseCSSPrefix + 'body-el',
        uiCls: 'body-el'
    }],

    setGroup: function (group) {
        var me = this,
            was = me._group;

        // We short-circuit the change detection because the content of the group
        // can change but yet the reference is the same...
        me._group = group;
        me.updateGroup(group, was);

        return me;
    },

    updateGroup: function (group) {
        var me = this,
            data, grouper, html, list, tpl;

        if (group) {
            list = me.parent;
            grouper = list.getStore().getGrouper();

            // See if the grouper belongs to this list and has a headerTpl override
            // in place (see Ext.grid.Column).
            tpl = (grouper && grouper.owner === list && grouper.headerTpl) || me.getTpl();

            if (tpl) {
                data = me.getGroupHeaderTplData();
                html = tpl.apply(data);
            }
        }

        me.setHtml(html || '\xa0');
    },

    getScrollerTarget: function () {
        return this.el;
    },

    doDestroy: function () {
        this.mixins.toolable.doDestroy.call(this);
        this.callParent();
    },

    privates: {
        augmentToolHandler: function (tool, args) {
            // args = [ itemHeader, tool, ev ]   ==>   [ list, info ]
            var info = args[1] = {
                event: args.pop(),
                group: this.getGroup(),
                itemHeader: args[0],
                tool: args[1]
            };

            args[0] = info.list = this.parent;
        },

        getGroupHeaderTplData: function (skipHtml) {
            var group = this.getGroup(),
                list = this.parent,
                data = group && {
                    name: group.getGroupKey(),
                    group: group,
                    groupField: list.getStore().getGrouper().getProperty(),
                    children: group.items,
                    count: group.length
                };

            if (data) {
                data.value = group.items[0].data[data.groupField];
            }

            if (!skipHtml) {
                data.html = Ext.htmlEncode(data.name);
            }

            // For Classic compat:
            data.groupValue = data.value;

            return data;
        },

        getList: function () {
            return this.parent; // backward compat
        },

        updateContentWidth: function (width) {
            var el = this._toolDockWrap || this.bodyElement;

            if (el) {
                el.setWidth(width ? width : null);
            }
        }
    }
});
