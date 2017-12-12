/**
 * This {@link Ext.Component#cfg!plugins plugin} adds pull to refresh functionality to the
 * {@link Ext.dataview.List list} component.
 *
 * ## Example
 *
 *      @example
 *      Ext.create({
 *          xtype: 'list',
 *          fullscreen: true,
 *
 *          plugins: {
 *              pullrefresh: {
 *                  pullText: 'Pull down for more new Tweets!'
 *              }
 *          },
 *
 *          itemTpl: [
 *              '<img src="{img}" alt="{name} photo" />',
 *              '<div class="tweet"><b>{name}:</b> {text}</div>'
 *          ],
 *
 *          store: [{
 *              name: 'Bill',
 *              img: 'https://www.sencha.com/forum/images/statusicon/forum_new-48.png',
 *              text: 'JavaScript development'
 *          }]
 *      });
 */
Ext.define('Ext.dataview.pullrefresh.PullRefresh', {
    extend: 'Ext.plugin.Abstract',
    alias: 'plugin.pullrefresh',
    alternateClassName: 'Ext.plugin.PullRefresh',

    mixins: [
        'Ext.mixin.ConfigProxy'
    ],

    proxyConfig: {
        widget: [
            /**
             * @cfg {String} lastUpdatedDateFormat
             * The format to be used on the last updated date.
             */
            'lastUpdatedDateFormat',

            /**
             * @cfg {String} lastUpdatedText
             * The text to be shown in front of the last updated time.
             */
            'lastUpdatedText',

            /**
             * @cfg {String} loadedText
             * The text that will be when data has been loaded.
             */
            'loadedText',

            /**
             * @cfg {String} loadingText
             * The text that will be shown while the list is refreshing.
             */
            'loadingText',

            /**
             * @cfg {String} pullText
             * The text that will be shown while you are pulling down.
             */
            'pullText',

            /**
             * @cfg {String} releaseText
             * The text that will be shown after you have pulled down enough to show the
             * release message.
             */
            'releaseText'
        ]
    },

    config: {
        /**
         * @cfg {Boolean} autoSnapBack
         * Determines whether the pulldown should automatically snap back after data has
         * been loaded. If `false` call {@link #snapBack} to manually snap the pulldown back.
         */
        autoSnapBack: true,

        /**
         * @cfg {Boolean} mergeData
         * `true` to insert new records into the store and to replace the data for
         * any incoming records that exist.
         *
         * `false` to completely overwrite store data with the fetched response.
         *
         * @since 6.2.1
         */
        mergeData: true,

        /**
         * @cfg {Boolean} overlay
         * `false` to move the list down to display the refresh indicator. `true` to float
         * the indicator over the top of the list with no movement.
         *
         * @since 6.2.1
         */
        overlay: false,

        /**
         * @cfg {Number} snappingAnimationDuration
         * The duration for snapping back animation after the data has been refreshed
         */
        snappingAnimationDuration: 300,

        //--------------------------
        // Private

        activateOffset: 0.75,

        widget: {
            lazy: true,
            $value: {
                xtype: 'pullrefreshbar'
            }
        },

        /**
         * @private
         */
        lastUpdated: new Date(),

        /**
         * @cfg {Ext.dataview.List} list
         * The list to which this PullRefresh plugin is connected.
         * This will usually by set automatically when configuring the list with this plugin.
         * @private
         */
        list: null,

        overshotMaxDistance: 50,

        /**
         * @private
         */
        state: 'pulling'
    },

    init: function (list) {
        this.setList(list);
    },

    destroy: function () {
        this.setList(null);
        this.callParent();
    },

    createWidget: function (config) {
        var ret = this.mergeProxiedConfigs('widget', config);

        // Since widget can be set as a string we'll lose all our properties that
        // we place in the config, so bolt on hidden here.
        ret.hidden = true;

        return ret;
    },

    privates: {
        overlayCls: Ext.baseCSSPrefix + 'pullrefresh-overlay',

        /**
         * Attempts to load the newest posts via the attached List's Store's Proxy
         * @private
         */
        fetchLatest: function() {
            this.getList().getStore().fetch({
                page: 1,
                start: 0,
                callback: this.onLatestFetched,
                scope: this
            });
        },

        reset: function() {
            var me = this,
                widget = me.getWidget();

            widget.setHidden(true);
            widget.setHeight(null);
            widget.setMinHeight(null);

            me.$measuredHeight = null;
        },

        /**
         * Snaps the List back to the top after a pullrefresh is complete
         * @param {Boolean} force Force the snapback to occur regardless of state {optional}
         * @private
         */
        snapBack: function (force) {
            var me = this,
                widget = me.getWidget(),
                state = me.getState(),
                hideAnimation = widget.getHideAnimation(),
                duration = me.getSnappingAnimationDuration();

            if (state === 'loaded' || force) {
                if (!hideAnimation) {
                    widget.el.animate({
                        preserveEndState: true,
                        duration: duration,
                        to: {
                            height: 0
                        },
                        callback: function() {
                            me.onSnapBackEnd(true);
                        }
                    });
                } else {
                    me.onSnapBackEnd();
                }
            }
        },

        //--------------------
        // Event handlers

        onDragEnd: function() {
            var me = this,
                state = me.getState(),
                widget = me.getWidget(),
                overshotMaxDistance = me.getOvershotMaxDistance();

            if (me.running) {
                me.running = false;
                if (state === 'holding') {
                    if (overshotMaxDistance) {
                        widget.el.animate({
                            duration: 75,
                            preserveEndState: true,
                            to: {
                                height: me.$measuredHeight
                            },
                            callback: function() {
                                me.setState('loading');
                                me.fetchLatest();
                            }
                        });
                    } else {
                        me.setState('loading');
                        me.fetchLatest();
                    }
                } else {
                    me.snapBack(true, false);
                }
            }
        },

        onDragMove: function(e) {
            var me = this,
                list = me.getList(),
                widget = me.getWidget(),
                listHeight = list.el.getHeight(),
                pullHeight = me.$measuredHeight,
                activateOffset = me.getActivateOffset() * pullHeight,
                overshotMaxDistance = me.getOvershotMaxDistance(),
                offset, overshot, overshotRange;

            if (me.running) {
                e.stopEvent();
                offset = e.getXY()[1] - me.startY;
                widget.setHidden(offset <= 0);

                if (offset > 0 && offset < pullHeight) {
                    me.setState('pulling');
                    widget.setHeight(offset);
                }
                else if (overshotMaxDistance && offset >= pullHeight) {
                    overshotRange = listHeight - pullHeight;
                    overshot = ((offset - pullHeight) / overshotRange);
                    widget.setHeight(pullHeight + (overshot * overshotMaxDistance));
                }

                // widget.setPull(Math.min(1, offset / activateOffset));
                widget.setPull(offset / activateOffset);

                if (offset >= activateOffset) {
                    me.setState('holding');
                }
            }
        },

        onDragStart: function(e) {
            var me = this,
                list = me.getList(),
                widget = me.getWidget(),
                dy;

            if (me.running) {
                e.stopEvent();
                return;
            }

            if (!me.$measuredHeight) {
                widget.setHidden(false);
                me.$measuredHeight = widget.el.getHeight();
                widget.setMinHeight(0);
                widget.setHidden(true);
            }

            dy = e.deltaY;

            if (list.getScrollable().getPosition().y === 0 && dy > 0 && dy > e.deltaX) {
                widget.setHidden(false);
                me.running = true;
                widget.setHeight(0);
                e.stopEvent();
            }
        },

        /**
         * Called after fetchLatest has finished grabbing data. Matches any returned
         * records against what is already in the Store. If there is an overlap, updates
         * the existing records with the new data and inserts the new items at the front
         * of the Store. If there is no overlap, insert the new records anyway and record
         * that there's a break in the timeline between the new and the old records.
         * @private
         */
        onLatestFetched: function(newRecords, operation, success) {
            var me = this,
                list = me.getList(),
                store = list.getStore(),
                length, toInsert,
                oldRecords, newRecord, oldRecord, i;

            if (success) {
                if (me.getMergeData()) {
                    oldRecords = store.getData();
                    toInsert = [];
                    length = newRecords.length;

                    for (i = 0; i < length; i++) {
                        newRecord = newRecords[i];
                        oldRecord = oldRecords.getByKey(newRecord.getId());

                        if (oldRecord) {
                            oldRecord.set(newRecord.getData());
                        } else {
                            toInsert.push(newRecord);
                        }
                    }

                    store.insert(0, toInsert);
                } else {
                    store.loadRecords(newRecords);
                }

                me.setLastUpdated(new Date());
            }
            me.setState('loaded');
            list.fireEvent('latestfetched', me, toInsert || newRecords);

            if (me.getAutoSnapBack()) {
                me.snapBack(true);
            }
        },

        /**
         * Called when PullRefresh has been snapped back to the top
         * @private
         */
        onSnapBackEnd: function(preventAnim) {
            var me = this,
                widget = me.getWidget();

            if (preventAnim) {
                widget.hide(null);
                me.setState('pulling');
                me.reset();
            } else {
                widget.hide();
                widget.on('hide', function() {
                    me.setState('pulling');
                    me.reset();
                }, me, {single: true});
            }
        },

        onTouchStart: function(e) {
            this.startY = e.getXY()[1];
        },

        //-------------------------
        // Configs

        // lastUpdated

        /**
         * @private
         */
        updateLastUpdated: function (value) {
            var widget = this.getWidget();

            if (widget) {
                widget.setLastUpdated(value);
            }
        },

        // list

        /**
         * @private
         */
        updateList: function (list, oldList) {
            var me = this,
                widget = me.widget;

            if (oldList) {
                oldList.el.un({
                    scope: me,
                    touchstart: 'onTouchStart',
                    dragstart: 'onDragStart',
                    drag: 'onDragMove',
                    dragend: 'onDragEnd'
                });
            }

            if (list) {
                list.el.on({
                    scope: me,
                    touchstart: 'onTouchStart',
                    dragstart: 'onDragStart',
                    drag: 'onDragMove',
                    dragend: 'onDragEnd'
                });

                if (widget) {
                    list.insert(0, widget);
                }
            }
        },

        // overlay

        updateOverlay: function (overlay) {
            var widget = this.getWidget();

            if (widget) {
                widget.el.toggleCls(this.overlayCls, overlay);
                widget.setTop(overlay ? 0 : null);
            }
        },

        // state

        /**
         * @private
         */
        updateState: function (value) {
            var widget = this.getWidget();

            if (widget) {
                widget.setState(value);
            }
        },

        // widget

        applyWidget: function (config, existing) {
            return Ext.updateWidget(existing, config, this, 'createWidget');
        },

        updateWidget: function (widget) {
            var me = this,
                list, overlay;

            // since we're lazy we store the reference to allow easy peeking
            me.widget = widget;

            if (widget) {
                overlay = me.getOverlay();
                me.updateOverlay(overlay);

                widget.setLastUpdated(me.getLastUpdated());
                widget.setState(me.getState());

                list = me.getList();

                if (list) {
                    list.insert(0, widget);
                }
            }
        }
    } // privates
});
