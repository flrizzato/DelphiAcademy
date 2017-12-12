/**
 * This class provides a convenient way to display a "popup" component to interact with
 * the user that is resizable, draggable and closable (similar to a browser popup window,
 * but contained in the normal Ext JS component tree). This means dialogs are not subject
 * to the restrictions of browser popup windows, but provide similar modal experiences.
 *
 *      var dialog = Ext.create({
 *          xtype: 'dialog',
 *          title: 'Dialog',
 *
 *          maximizable: true,
 *          html: 'Content<br>goes<br>here',
 *
 *          buttons: {
 *              ok: function () {  // standard button (see below)
 *                  dialog.destroy();
 *              }
 *          }
 *      });
 *
 *      dialog.show();
 *
 * The above use of `buttons` is a {@link Ext.Container#cfg!weighted weighted} container.
 * This form allows the Ext JS config system to merge properties by aligning on `itemId`
 * as the object keys (`'ok'` in this case). This merging capability enables the use of
 * `standardButtons` but is also a powerful technique for leveraging class inheritance
 * in your views.
 *
 * ## Standard Buttons
 *
 * The main advantage of using the `buttons` config is the availability of
 * {@link Ext.Panel#cfg!standardButtons standardButtons}. The `standardButtons` config
 * describes many common buttons (such as `ok` above) and provides their `text` as well
 * as the proper, platform-specific ordering.
 *
 * Custom buttons can be mixed with standard buttons or can fully replace them:
 *
 *      buttons: {
 *          ok: 'onOK',
 *
 *          verify: {
 *              text: 'Verify',
 *              handler: 'onVerify',
 *              weight: 200
 *          }
 *      }
 *
 * When combined, custom buttons are presented first. In the above, the `weight` config
 * is used to order the Verify button after the OK button. The weights assigned to the
 * {@link Ext.Panel#cfg!standardButtons standardButtons} vary by platform but `200` is
 * beyond their range.
 *
 * ## Handling ESC and Close
 *
 * Many dialogs have a `Cancel` button (or equivalent) that closes the dialog without
 * taking action. In some cases this action is first confirmed to avoid data loss.
 *
 * A common problem when implementing dialogs is the presence of these other two means to
 * dismiss the dialog since they often bypass the button handler that is expected to be
 * used to achieve an orderly shutdown.
 *
 * With `Ext.Dialog`, both the ESC key and `close` tool handler call the `close` method
 * to dismiss the dialog. The `close` method (and its `closeAction` config) are enhanced
 * versions of the implementation in `Ext.Panel`.
 *
 * The default dismiss sequence uses the `dismissAction` config to identify the candidate
 * `buttons`.  The most common match here is the `Cancel` button. If there is a matching
 * button then that button's `handler` is called just as if the user had clicked on it
 * instead.
 *
 * The end result is that when using `standardButtons` such as `cancel` or `close`, you
 * seldom need to worry about ESC or `close` tool inconsistency. The handler for your
 * button will be called in all cases.
 *
 * ### Custom Buttons and Options
 *
 * If the dialog has custom buttons, the `dismissHandler` config can be used to direct
 * `close` to a suitable method. Ideally this would be the same method connected to the
 * corresponding button.
 *
 *      buttons: {
 *          goAway: {
 *              text: 'Go Away!',
 *              handler: 'onGoAway'
 *          }
 *      },
 *
 *      dismissHandler: 'onGoAway'
 *
 * To simply allow the `closeAction` config to call `hide` or `destroy` methods for ESC
 * and `close`, do the following:
 *
 *      dismissHandler: true
 *
 * The {@link #method!close close method} will fire the {@link #event!beforeclose beforeclose}
 * and {@link #event!close close} events in any case. Using the `closeAction` approach
 * exposes dialogs to this alternate shutdown sequence but can be enabled as above for
 * simple use cases.
 *
 * ## Maximize / Restore
 *
 * The ability to `maximize` (fill the viewport) with the dialog can be quite useful for
 * complex popups. This can take two forms:
 *
 *  - The `maximizable` config to provide a {@link Ext.Tool tool} to `maximize` and also
 *   to `restore` the dialog.
 *  - The `maximized` config to control the current state.
 *
 * The `maximized` config can be used directly if the `maximizeTool` is not desired. In
 * other words, the ability to control the `maximized` config is not dependent on whether
 * `maximizable` is set or not.
 *
 * ### Note
 * This class is analogous to the Ext JS Classic Toolkit's 'Ext.window.Window' class. This
 * class has those names (`Ext.Window` and `Ext.window.Window`) as alternate class names
 * and the `window` xtype for compatibility sake.
 * @since 6.5.0
 */
Ext.define('Ext.Dialog', {
    extend: 'Ext.Panel',
    xtype: [
        'dialog',
        'window'  // classic compat
    ],
    alternateClassName: [ 'Ext.Window', 'Ext.window.Window' ], // classic compat

    requires: [
        'Ext.Deferred',
        'Ext.drag.proxy.Original'
    ],

    /**
     * @property {Boolean} isDialog
     * `true` in this class to identify an object this type, or subclass thereof.
     */
    isDialog: true,
    isWindow: true,  // classic compat

    /**
     * @property ariaRole
     * @inheritdoc
     */
    ariaRole: 'dialog',

    /**
     * @property classCls
     * @inheritdoc
     */
    classCls: Ext.baseCSSPrefix + 'dialog',

    /**
     * @event beforemaximize
     * Fires before maximizing the dialog. Returning `false` from this event will cancel
     * the maximization.
     * @param {Ext.Dialog} dialog
     */

    /**
     * @event beforerestore
     * Fires before restoring the dialog. Returning `false` from this event will cancel
     * the restoration.
     * @param {Ext.Dialog} dialog
     */

    /**
     * @event maximize
     * Fires after the dialog has been maximized. If there is a `maximizeAnimation` this
     * event will fire after the animation is complete.
     * @param {Ext.Dialog} dialog
     */

    /**
     * @event restore
     * Fires after the dialog has been restored to its original size. If there is a
     * `restoreAnimation` this event will fire after the animation is complete.
     * @param {Ext.Dialog} dialog
     */

    cachedConfig: {
        /**
         * @cfg {String/String[]} dismissAction
         * This config lists one or more `itemId` values to look for in this dialog's
         * `buttons`. The first button to be found from this list will be invoked in
         * response to the ESC key or the `close` tool.
         *
         * This config is ignored if a `dismissHandler` is specified.
         *
         * @since 6.5.0
         */
        dismissAction: [ 'cancel', 'abort', 'no', 'close' ],

        /**
         * @cfg {Object} maximizeAnimation
         * The animation configuration to use when maximizing.
         *
         * @since 6.5.0
         */
        maximizeAnimation: {
            easing: 'ease-in',
            from: {
                opacity: 0.6
            },
            to: {
                opacity: 1
            }
        },

        /**
         * @cfg {Object/Ext.Dialog} maximizeProxy
         * Configuration options for a proxy dialog to animate to/from maximized state.
         * The `title`, `iconCls`, `ui`, `cls` and `userCls` will be copied to the proxy.
         *
         * @since 6.5.0
         */
        maximizeProxy: {
            centered: false,
            draggable: false,
            modal: false,
            showAnimation: null,
            hideAnimation: null
        },

        /**
         * @cfg {Object/Ext.Tool} maximizeTool
         * Configuration options for the `maximize` tool.
         *
         * @since 6.5.0
         */
        maximizeTool: {
            itemId: 'maximize',  // this will also set "type"
            tooltip: 'Maximize to fullscreen'
        },

        /**
         * @cfg {Object} restoreAnimation
         * The animation configuration to use when restoring to normal size.
         *
         * @since 6.5.0
         */
        restoreAnimation: {
            easing: 'ease-in',
            from: {
                opacity: 1
            },
            to: {
                opacity: 0.6
            }
        },

        /**
         * @cfg {Object/Ext.Tool} restoreTool
         * Configuration options for the `restore` tool.
         *
         * @since 6.5.0
         */
        restoreTool: {
            itemId: 'restore',  // this will also set "type"
            tooltip: 'Restore to original size'
        }
    },

    config: {
        /**
         * @cfg {Boolean/Ext.drag.Constraint} constrainDrag
         * Set to `false` to not constrain the dialog to the viewport.
         *
         * @since 6.5.0
         */
        constrainDrag: true,

        /**
         * @cfg {String/Function} dismissHandler
         * The function or controller method name to call on ESC key press or `close`
         * tool click.
         *
         * If this config is specified, `dismissAction` will be ignored.
         *
         * @controllable
         * @since 6.5.0
         */
        dismissHandler: null,

        /**
         * @cfg {Boolean} [maximizable=false]
         * Set to `true` to display the 'maximizeTool` to allow the user to maximize the
         * dialog. Note that when a dialog is maximized, the `maximizeTool` is replaced
         * with the `restoreTool` to give the user the ability to restore the dialog to
         * its previous size.
         *
         * This config only controls the presence of the `maximize` and `restore` tools.
         * The dialog can always be set to `maximized` by directly setting the config or
         * calling the `maximize` and `restore` methods.
         *
         * @since 6.5.0
         */
        maximizable: null,

        /**
         * @cfg {Boolean} [maximized=false]
         * Set to `true` to display the dialog in a maximized state. Changing this config
         * after construction will utilize the `maximizeAnimation` or `restoreAnimation`.
         *
         * These can be avoided by passing `null` to `maximize` or `restore` methods:
         *
         *      dialog.setMaximized(true);  // uses maximizeAnimation
         *      // or:
         *      dialog.maximize(null);      // no animation for this change
         *
         *      dialog.setMaximized(false); // uses restoreAnimation
         *      // or:
         *      dialog.restore(null);       // no animation for this change
         *
         * @since 6.5.0
         */
        maximized: null,

        /**
         * @cfg {String/Function} maskTapHandler
         * The function or method name to call when the modal mask is tapped. A common use
         * for this config is to cancel the dialog.
         *
         *      Ext.create({
         *          xtype: 'dialog',
         *
         *          buttons: {
         *              ok: 'onOK',
         *              cancel: 'onCancel'
         *          },
         *
         *          maskTapHandler: 'onCancel'
         *      });
         *
         * @controllable
         * @since 6.5.0
         */
        maskTapHandler: null,

        /**
         * @cfg {Boolean} restorable
         * This config is used when the dialog is `maximized` to show the `restoreTool`.
         *
         * @since 6.5.0
         * @private
         */
        restorable: null

        // minimizable: null,
        // minimized: null,
    },

    /**
     * @cfg border
     * @inheritdoc
     */
    border: true,
    
    /**
     * @cfg bodyBorder
     * @inheritdoc
     */
    bodyBorder: false,
    
    /**
     * @cfg centered
     * @inheritdoc
     */
    centered: true,
    
    /**
     * @cfg floated
     * @inheritdoc
     */
    floated: true,
    
    /**
     * @property focusable
     * @inheritdoc
     */
    focusable: false,
    
    /**
     * @cfg tabIndex
     * @inheritdoc
     */
    tabIndex: -1,

    /**
     * @cfg draggable
     * @inheritdoc
     */
    draggable: {
        handle: '.' + Ext.baseCSSPrefix + 'draggable',
        listeners: {
            beforedragstart: 'onBeforeDragDialog',
            scope: 'this'
        }
    },

    /**
     * @cfg keyMap
     * @inheritdoc
     */
    keyMap: {
        ESC: 'onEscape',
        scope: 'this'
    },

    /**
     * @cfg modal
     * @inheritdoc
     */
    modal: true,

    /**
     * @cfg shadow
     * @inheritdoc
     */
    shadow: true,

    headerCls: Ext.baseCSSPrefix + 'dialogheader',
    titleCls: Ext.baseCSSPrefix + 'dialogtitle',
    toolCls: [
        Ext.baseCSSPrefix + 'paneltool',
        Ext.baseCSSPrefix + 'dialogtool'
    ],
    
    /**
     * @cfg hideMode
     * @inheritdoc
     */
    hideMode: 'offsets',

    /**
     * @cfg hideAnimation
     * @inheritdoc
     */
    hideAnimation: {
        type: 'popOut',
        duration: 250,
        easing: 'ease-out'
    },

    /**
     * @cfg showAnimation
     * @inheritdoc
     */
    showAnimation: {
        type: 'popIn',
        duration: 150,
        easing: 'ease-out'
    },

    //------------------------------

    initialize: function() {
        var me = this;

        me.callParent();

        if (me.tabGuard) {
            me.addPlugin({
                type: 'tabguard',
                tabGuardBeforeIndex: me.tabGuardBeforeIndex,
                tabGuardAfterIndex: me.tabGuardAfterIndex
            });
        }
    },

    doDestroy: function () {
        Ext.destroy(this.maximizeTool, this.restoreTool);

        this.callParent();
    },

    close: function (event) {
        var me = this,
            buttons = me.getButtons(),
            actions = me.getDismissAction(),
            handler = me.getDismissHandler(),
            action, closeAction, closeActionIsDestroy, done, i, n;

        event = event || null;

        if (me.fireEvent('beforeclose', me, event) !== false) {
            // if dismissHandler:true is given we'll just use closeAction to drive
            // things. Otherwise, call the dismissHandler or faux-tap the appropriate
            // button (if present).
            if (handler !== true) {
                if (handler) {
                    Ext.callback(handler, null, [me, event], 0, me);
                    done = true;
                }
                else if (actions && buttons) {
                    if (typeof actions === 'string') {
                        actions = [actions];
                    }

                    for (i = 0, n = actions.length; i < n; ++i) {
                        action = buttons.getComponent(actions[i]);

                        if (action && action.isButton) {
                            action.onTap(event);
                            done = true;
                            break;
                        }
                    }
                }
            }

            // If nothing so far, fallback to closeAction (defaults to "destroy").
            if (!done) {
                closeAction = me.getCloseAction();

                if (closeAction) {
                    if (!(closeActionIsDestroy = closeAction === 'destroy')) {
                        me[closeAction]();
                    }
                }
            }

            if (!me.destroyed) {
                me.fireEvent('close', me, event);

                if (closeActionIsDestroy) {
                    me.destroy();
                }
            }
        }
    },

    createMaximizeProxy: function (config) {
        var me = this;

        return Ext.apply({
            title: me.getTitle(),
            cls: me.getCls(),
            userCls: me.getUserCls(),
            iconCls: me.getIconCls(),
            ui: me.getUi()
        }, config);
    },

    /**
     * Maximizes this dialog by setting the `maximized` config to `true`. This method
     * allows the `maximizeAnimation` to be skipped or altered using the `animation`
     * parameter. This parameter applies only to this maximize transition.
     *
     * @param {Object} animation A one-time replacement for `maximizeAnimation`.
     * @return {Promise} A promise that resolves when the animation is complete.
     * @since 6.5.0
     */
    maximize: function (animation) {
        var me = this,
            maximizing = me.maximizing;

        if (!maximizing && !me.getMaximized()) {
            me._maximizeAnim = animation;
            me.setMaximized(true);

            if (!(maximizing = me.maximizing)) {
                return Ext.Promise.resolve(false);
            }
        }

        return maximizing ? maximizing.promise : Ext.Promise.resolve(true);
    },

    /**
     * Restores this dialog by clearing the `maximized` config to `false`. This method
     * allows the `restoreAnimation` to be skipped or altered using the `animation`
     * parameter. This parameter applies only to this restore transition.
     *
     * @param {Object} animation A one-time replacement for `restoreAnimation`.
     * @return {Promise} A promise that resolves when the animation is complete.
     * @since 6.5.0
     */
    restore: function (animation) {
        var me = this,
            restoring = me.restoring;

        if (!restoring && me.getMaximized()) {
            me._maximizeAnim = animation;
            me.setMaximized(false);

            if (!(restoring = me.restoring)) {
                return Ext.Promise.resolve(false);
            }
        }

        return restoring ? restoring.promise : Ext.Promise.resolve(true);
    },

    shouldRecenter: function () {
        return !this.getMaximized() && this.callParent();
    },

    //------------------------------
    // Configs

    // constrainDrag

    updateConstrainDrag: function (constrain) {
        var dragger = this.getDraggable();

        if (dragger) {
            if (constrain === true) {
                constrain = Ext.getBody();
            }

            dragger.setConstrain(constrain);
        }
    },

    // draggable

    updateDraggable: function (draggable, existing) {
        this.callParent([ draggable, existing ]);

        if (!this.isConfiguring) {
            this.syncHeaderItems();
        }
    },

    // header

    updateHeader: function (header, oldHeader) {
        var me = this,
            beforeGuard;
        
        me.callParent([ header, oldHeader ]);

        if (header) {
            me.syncHeaderItems();
            
            if (me.tabGuard && me.getTabGuard) {
                beforeGuard = me.getTabGuard('before');
                
                // We need to keep top tab guard at the top of the DOM order
                if (beforeGuard && beforeGuard.dom) {
                    beforeGuard.insertBefore(header.el);
                }
            }
        }
    },

    // maximizable

    applyMaximizable: function (maximizable) {
        var me = this;

        me.maximizeTool = Ext.updateWidget(me.maximizeTool, maximizable,
            me, 'createMaximizeTool', 'maximizeTool');

        me.syncHeaderItems();

        return maximizable;
    },

    // maximized

    applyMaximized: function (maximized) {
        var me = this,
            event;

        if (!me.isConfiguring) {
            event = maximized ? 'beforemaximize' : 'beforerestore';

            if (me.fireEvent(event, me) === false) {
                // Clear any animation override from maximize() or restore()
                me._maximizeAnim = undefined;

                return;  // rejected... so do not change the config
            }
        }

        return !!maximized;
    },

    updateMaximized: function (maximized) {
        var me = this,
            el = me.el,
            maximizedCls = me.maximizedCls,
            maximizeTool = me.maximizeTool,
            pendingName = maximized ? 'restoring' : 'maximizing',
            pending = me[pendingName],
            after, anim, before, center;

        if (me.isConfiguring) {
            me.needsCenter = maximized;
        }
        else {
            anim = me._maximizeAnim;
            center = me.needsCenter && !maximized;
            me.needsCenter = false;

            if (anim === undefined) {
                anim = me[maximized ? 'getMaximizeAnimation' : 'getRestoreAnimation']();
            }
        }

        me._maximizeAnim = undefined; // null disables the animation

        if (pending) {
            pending.destroy(); // this pushes anim to end and calls our callback
        }

        if (me.getMaximizable()) {
            me.setRestorable(maximized);
        }
        else {
            // This is done by applyRestorable but we need to update x-draggable even
            // if we aren't presenting those tools.
            me.syncHeaderItems();
        }

        if (maximizeTool) {
            maximizeTool.setHidden(maximized);
        }

        if (!anim) {
            el.toggleCls(maximizedCls, maximized);

            if (center) {
                me.center();
            }

            me.fireEvent(maximized ? 'maximize' : 'restore', me);
        }
        else {
            if (maximized) {
                pendingName = 'maximizing';

                // When we are maximizing, we need the current size (before) and the
                // viewport size (after). We don't add the x-maximized class until
                // after the animation.
                before = me.captureSize();
                after = me.captureSize(true);
            }
            else {
                pendingName = 'restoring';

                // When restoring, we snap the dialog to the restored size immediately
                // and animate the proxy from fullscreen down to that place.
                el.removeCls(maximizedCls);

                if (center) {
                    me.center();
                }

                before = me.captureSize(true);
                after = me.captureSize();
            }

            me[pendingName] = me.animateMaximizeRestore(before, after, anim, function () {
                if (maximized) {
                    // Now that the proxy has animated up and is gone, snap the dialog
                    // to full screen.
                    el.addCls(maximizedCls);
                }

                me[pendingName] = null;

                me.fireEvent(maximized ? 'maximize' : 'restore', me);
            });
        }
    },

    // maximizeTool

    createMaximizeTool: function (config) {
        var tool = this.adjustToolDefaults(Ext.clone(config));

        tool.handler = 'onMaximize';
        tool.scope = this;

        return tool;
    },

    // restorable

    applyRestorable: function (restorable) {
        var me = this;

        me.restoreTool = Ext.updateWidget(me.restoreTool, restorable,
            me, 'createRestoreTool', 'restoreTool');

        me.syncHeaderItems();

        return restorable;
    },

    createRestoreTool: function (config) {
        var tool = this.adjustToolDefaults(Ext.clone(config));

        tool.handler = 'onRestore';
        tool.scope = this;

        return tool;
    },

    //-----------------------------------------------------------

    afterShow: function () {
        this.callParent();
        if (this.getModal()) {
            this.focus();
        }
    },

    onBeforeDragDialog: function (draggable, info, event) {
        var header = this.getHeader();

        // The "handle" of x-draggable could match a child item... so reject any
        // drag outside of our header.
        if (!header || !header.el.contains(event.target)) {
            return false;
        }
    },

    onCloseTool: function (dialog, tool, event) {
        this.close(event);
    },

    onEscape: function (event) {
        this.close(event);
    },

    onMaximize: function () {
        this.setMaximized(true);
    },

    onRestore: function () {
        this.setMaximized(false);
    },

    onModalMaskTap: function (e) {
        var me = this,
            handler = me.getMaskTapHandler(),
            ret;

        if (handler) {
            Ext.callback(handler, null, [ me, e ], 0, me);
        }
        else {
            ret = me.callParent([ e ]);  // to respect hideOnMaskTap config
        }

        return ret;
    },

    //-----------------------------------------------------------

    privates: {
        draggableCls: Ext.baseCSSPrefix + 'draggable',
        needsCenter: false,
        maximizedCls: Ext.baseCSSPrefix + 'maximized',

        animateMaximizeRestore: function (before, after, anim, callback) {
            var me = this,
                pending = new Ext.Deferred(),
                proxy = me.getMaximizeProxy(),
                a = Ext.merge({
                    // duration: 3000,
                    from: {
                        width: before.w + 'px',
                        height: before.h + 'px',
                        transform: {
                            translateX: before.x + 'px',
                            translateY: before.y + 'px'
                        }
                    },

                    to: {
                        width: after.w + 'px',
                        height: after.h + 'px',
                        transform: {
                            translateX: after.x + 'px',
                            translateY: after.y + 'px'
                        }
                    }
                }, anim);

            proxy = me.createMaximizeProxy(proxy);
            proxy = new me.self(proxy);
            proxy.show();

            a.element = proxy.el;
            a.callback = function () {
                proxy.destroy();
                callback();
                pending.resolve(true);
            };

            a = new Ext.fx.animation.Abstract(a);
            Ext.Animator.run(a);

            pending.destroy = function () {
                pending.destroy = Ext.emptyFn;
                a.destroy();
                pending.destroyed = true;
            };

            return pending;
        },

        captureSize: function (maximized) {
            if (maximized) {
                return {
                    x: 0,
                    y: 0,
                    w: Ext.getViewportWidth(),
                    h: Ext.getViewportHeight()
                };
            }

            var me = this,
                size = me.el.measure();

            return {
                x: me.getX(),
                y: me.getY(),
                w: size.width,
                h: size.height
            };
        },

        syncHeaderItems: function () {
            var me = this,
                maximizeTool = me.maximizeTool,
                restoreTool = me.restoreTool,
                header = (maximizeTool || restoreTool) ? me.ensureHeader() : me.getHeader(),
                draggableCls = me.draggableCls,
                draggable, title;

            if (header) { // header:false will never allow a header to be created
                draggable = me.getDraggable() && !me.getMaximized();

                header.toggleCls(draggableCls, draggable);

                title = header.getTitle();
                if (title) {
                    title.toggleCls(draggableCls, draggable);
                }

                if (maximizeTool && maximizeTool.parent !== header) {
                    header.add(maximizeTool);
                }

                if (restoreTool && restoreTool.parent !== header) {
                    header.add(restoreTool);
                }
            }
        },

        updateX: function (x, oldX) {
            this.callParent([ x, oldX ]);

            // a true user-drag will always involve x & y so we just need to act on
            // one of them...
            if (!this._centering && this.getCentered()) {
                this.setCentered(false);
            }
        }
    } // privates
});
