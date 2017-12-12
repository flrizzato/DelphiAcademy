/**
 * @class Ext.Progress
 *
 *     @example
 *     Ext.create({
 *         xtype: 'grid',
 *         title: 'Simpsons',
 *         store: {
 *             data: [
 *                 { name: 'Lisa', progress: .159 },
 *                 { name: 'Bart', progress: .216 },
 *                 { name: 'Homer', progress: .55 },
 *                 { name: 'Maggie', progress: .167 },
 *                 { name: 'Marge', progress: .145 }
 *             ]
 *         },
 *         columns: [
 *             { text: 'Name',  dataIndex: 'name' },
 *             {
 *                 text: 'Progress',
 *                 width: 120,
 *                 dataIndex: 'progress',
 *                 cell: {
 *                     xtype: 'widgetcell',
 *                     widget: {
 *                         xtype: 'progress'
 *                     }
 *                 }
 *             }
 *         ],
 *         height: 200,
 *         width: 400,
 *         fullscreen: true
 *     });
 */

Ext.define('Ext.overrides.Progress', {
    override: 'Ext.Progress',

    requires: ['Ext.fx.Animation'],

    initialize: function() {
        this.callParent();

        this.on('painted', 'onPainted', this);
    },

    onPainted: function() {
      this.syncWidth();
    },

    onResize: function (width) {
        this.syncWidth(width);
    },

    syncWidth: function (width) {
        var me = this;

        if (width == null) {
            width = me.element.getWidth();
        }

        me.backgroundEl.setWidth(width);
        me.textEl.setWidth(width);
    },

    privates: {
        startBarAnimation: function(o) {
            var me = this;

            me.barAnim = new Ext.fx.Animation(Ext.apply(o, {
                element: me.barEl,
                preserveEndState: true,
                callback: function() {
                    delete me.barAnim;
                }
            }));
            Ext.Animator.run(me.barAnim);
        },

        stopBarAnimation: function() {
            var barAnim = this.barAnim;
            if (barAnim) {
                barAnim.destroy();
            }
            this.barAnim = null;
        }
    }
});
