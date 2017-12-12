/**
 * A 'Toast' is a simple message that is displayed on the screen and then automatically closed by a
 * timeout. Think about it like a text only alert box that will self destruct. **A Toast should not
 * be instantiated manually** but creating by calling 'Ext.toast(message, timeout)'. This will create
 * one reusable toast container and content will be swapped out as toast messages are queued
 * or displayed.
 *
 * # Simple Toast
 *
 *      @example
 *      // Toast will close in 1000 milliseconds (default)
 *      Ext.toast('Hello Sencha!');
 *
 * # Toast with Timeout
 *
 *      @example
 *      // Toast will close in 5000 milliseconds
 *      Ext.toast('Hello Sencha!', 5000);
 *
 * # Toast with config
 *
 *      @example
 *      // Toast will close in 2000 milliseconds
 *      Ext.toast({message: 'Hello Sencha!', timeout: 2000});
 *
 * # Multiple Toasts queued
 *
 *      @example
 *      Ext.toast('Hello Sencha!');
 *      Ext.toast('Hello Sencha Again!');
 *      Ext.toast('Hello Sencha One More Time!');
 */
Ext.define('Ext.Toast', {
    extend: 'Ext.Sheet',
    requires: [
        'Ext.util.InputBlocker'
    ],

    config: {
        /**
         * @cfg centered
         * @inheritdoc
         */
        centered: false,

        /**
         * @cfg showAnimation
         * @inheritdoc
         */
        showAnimation: {
            type: 'popIn',
            duration: 250,
            easing: 'ease-out'
        },

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
         * Override the default `zIndex` so it is normally always above positioned components.
         */
        zIndex: 999,

        /**
         * @cfg {String} message
         * The message to be displayed in the {@link Ext.Toast}.
         * @accessor
         */
        message: '',

        /**
         * @cfg {Number} timeout
         * The amount of time in milliseconds to wait before destroying the toast automatically
         */
        timeout: 1000,

        /**
         * @cfg {Number} maxQueue
         * The the maximum number of toasts that can be queued up.  When there are this many toasts queued up and
         * a new call to Ext.toast() is made, the oldest queued Toast that is not yet displayed will be dropped
         * from the queue.
         */
        maxQueue: 3,

        /**
         * @cfg {Boolean/Object} messageAnimation
         * The animation that should be used between toast messages when they are queued up
         */
        messageAnimation: true,

        /**
         * @cfg hideOnMaskTap
         * @inheritdoc
         */
        hideOnMaskTap: true,

        /**
         * @cfg modal
         * @hide
         */
        modal: false,

        /**
         * @cfg layout
         * @inheritdoc
         */
        layout: {
            type: 'vbox',
            pack: 'center'
        }
    },

    /**
     * @property classCls
     * @inheritdoc
     */
    classCls: Ext.baseCSSPrefix + 'toast',

    /**
     * @private
     */
    applyMessage: function (value) {
        var config = {
            html: value,
            cls: this.baseCls + '-text'
        };

        return Ext.factory(config, Ext.Component, this._message);
    },

    /**
     * @private
     */
    updateMessage: function (newMessage) {
        if (newMessage) {
            this.add(newMessage);
        }
    },

    /**
     * @private
     */
    startTimer: function () {
        var timeout = this.getTimeout();
        if (this._timeoutID) {
            Ext.undefer(this._timeoutID);
        }

        if (!Ext.isEmpty(timeout)) {
            this._timeoutID = Ext.defer(this.onTimeout.bind(this), timeout);
        } else {
            this.onTimeout();
        }
    },

    stopTimer: function () {
        Ext.undefer(this._timeoutID);
        this._timeoutID = null;
    },

    /**
     * @method
     * @private
     */
    next: Ext.emptyFn,

    getIsAnimating: function () {
        var messageContainer = this.getMessage();
        return (messageContainer && Ext.Animator.hasRunningAnimations(messageContainer)) || Ext.Animator.hasRunningAnimations(this);
    },

    /**
     * @private
     */
    showToast: function (config) {
        var me = this,
            message = config.message,
            timeout = config.timeout,
            messageContainer = me.getMessage(),
            msgAnimation = me.getMessageAnimation();

        // If the toast has already been rendered and is visible on the screen
        if (me.isRendered() && me.isHidden() === false) {
            messageContainer.onAfter({
                // After the hide is complete
                hiddenchange: function () {
                    me.setMessage(message);
                    me.setTimeout(timeout);
                    messageContainer.onAfter({
                        scope: me,
                        // After the show is complete
                        hiddenchange: function () {
                            me.startTimer();
                        },
                        single: true
                    });
                    messageContainer.show(msgAnimation);
                },
                scope: me,
                single: true
            });

            messageContainer.hide(msgAnimation);
        } else {
            Ext.util.InputBlocker.blockInputs();

            //if it has not been added to a container, add it to the Viewport.
            if (!me.getParent() && Ext.Viewport) {
                Ext.Viewport.add(me);
            }

            me.setMessage(message);
            me.setTimeout(timeout);
            me.startTimer();
            me.show({
                animation: null,
                alignment: {
                    component: document.body,
                    alignment: 't-t',
                    options: {
                        offset: [0, 20]
                    }
                }
            });
        }
    },

    /**
     * @private
     */
    beforeHide: function (animation) {
        this.callParent(arguments);

        // If the message is animating cancel this hide
        if (this.getIsAnimating()) {
            return false;
        }

        this.stopTimer();
        if (!this.next()) {
            return false;
        }
    },

    /**
     * @private
     */
    onTimeout: function () {
        if (this._timeoutID !== null) {
            this.hide();
        }
    },

    doDestroy: function() {
        this.stopTimer();
        this.callParent();
    }
}, function (Toast) {
    var _queue = [];

    function getInstance() {
        if (!Ext.Toast._instance) {
            Ext.Toast._instance = Ext.create('Ext.Toast');
        }
        return Ext.Toast._instance;
    }

    //<debug>
    /**
     * @member Ext.Toast
     * @method getQueueCount
     * @private
     * Provided for unit tests
     * @returns {Number}
     */
    Toast.prototype.getQueueCount = function() {
        return _queue.length;
    };
    //</debug>

    Toast.prototype.next = function () {
        var config = _queue.shift();

        if (config) {
            this.showToast(config);
        }

        return !config;
    };

    /**
     * Destroys any Toast components and elements, freeing the resources.
     *
     * They will be created again upon calling Ext.toast().
     */
    Ext.Toast.destroy = function() {
        if (Ext.Toast._instance) {
            Ext.Toast._instance.destroy();
            Ext.Toast._instance = null;
        }
    };
    Ext.toast = function (message, timeout) {
        var toast = getInstance(),
            maxQueue = Ext.Toast.prototype.config.maxQueue,
            config = message;

        if (Ext.isString(message)) {
            config = {
                message: message,
                timeout: timeout
            };
        }

        //<debug>
        if (!config) {
            throw new Error("Toast requires a message");
        }
        //</debug>

        if (config.timeout === undefined) {
            config.timeout = Ext.Toast.prototype.config.timeout;
        }

        _queue.push(config);

        if (_queue.length > maxQueue) {
            _queue.shift();
        }

        if (!toast.isRendered() || toast.isHidden()) {
            toast.next();
        }

        return toast;
    }
});

