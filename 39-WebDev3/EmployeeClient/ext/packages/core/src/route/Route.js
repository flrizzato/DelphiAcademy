/**
 * Enables reactive actions to handle changes in the hash by using the
 * {@link Ext.route.Mixin#routes routes} configuration in a controller.
 * An example configuration would be:
 *
 *     Ext.define('MyApp.view.main.MainController', {
 *         extend: 'Ext.app.ViewController',
 *         alias: 'controller.app-main',
 *
 *         routes: {
 *             'user/:id': 'onUser'
 *         },
 *
 *         onUser: function (id) {
 *             // ...
 *         }
 *     });
 *
 * The `routes` object can also receive an object to further configure
 * the route, for example you can configure a `before` action that will
 * be executed before the `action` or can cancel the route execution:
 *
 *     Ext.define('MyApp.view.main.MainController', {
 *         extend: 'Ext.app.ViewController',
 *         alias: 'controller.app-main',
 *
 *         routes: {
 *             'user/:id': {
 *                 action: 'onUser',
 *                 before: 'onBeforeUser'
 *             }
 *         },
 *
 *         onBeforeUser: function (id) {
 *             return new Ext.Promise(function (resolve, reject) {
 *                 Ext.Ajax
 *                     .request({
 *                         url: '/check/permission',
 *                         params: {
 *                             route: 'user',
 *                             meta: {
 *                                 id: id
 *                             }
 *                         }
 *                     })
 *                     .then(resolve, reject);
 *             });
 *         },
 *
 *         onUser: function (id) {
 *             // ...
 *         }
 *     });
 */
Ext.define('Ext.route.Route', {
    requires: [
        'Ext.route.Action'
    ],

    /**
     * @event beforeroute
     * @member Ext.GlobalEvents
     *
     * Fires when a route is about to be executed. This allows pre-processing to add additional
     * {@link Ext.route.Action#before before} or {@link Ext.route.Action#action action} handlers
     * when the {@link Ext.route.Action Action} is run.
     *
     * The route can be prevented from executing by returning `false` in a listener
     * or executing the {@link Ext.route.Action#stop stop} method on the action.
     *
     * @param {Ext.route.Route} route The route being executed.
     * @param {Ext.route.Action} action The action that will be run.
     */

    config: {
        /**
         * @cfg {String} name The name of this route. The name can be used when using
         * {@link Ext.route.Mixin#redirectTo}.
         */
        name: null,

        /**
         * @cfg {String} url (required) The url regex to match against.
         */
        url: null,

        /**
         * @cfg {Boolean} [allowInactive=false] `true` to allow this route to be triggered on
         * a controller that is not active.
         */
        allowInactive: false,

        /**
         * @cfg {Boolean} [caseInsensitive=false] `true` to allow the tokens to be matched with
         * case-insensitive.
         */
        caseInsensitive: false,

        /**
         * @private
         * @cfg {Object[]} [handler=[]] The array of connected handlers to this route. Each handler
         * must defined a `scope` and can define an `action`, `before` and/or `exit` handler:
         *
         *     handlers: [{
         *         action: function() {
         *             //...
         *         },
         *         scope: {}
         *     }, {
         *         action: function() {
         *             //...
         *         },
         *         before: function() {
         *             //...
         *         },
         *         scope: {}
         *     }, {
         *         exit: function() {
         *             //...
         *         },
         *         scope: {}
         *     }]
         *
         * The `action`, `before` and `exit` handlers can be a string that will be resolved
         * from the `scope`:
         *
         *     handlers: [{
         *         action: 'onAction',
         *         before: 'onBefore',
         *         exit: 'onExit',
         *         scope: {
         *             onAction: function () {
         *                 //...
         *             },
         *             onBefore: function () {
         *                 //...
         *             },
         *             onExit: function () {
         *                 //...
         *             }
         *         }
         *     }]
         */
        handlers: []
    },

    /**
     * @cfg {Object} conditions Optional set of conditions for each token in the url
     * string. Each key should be one of the tokens, each value should be a regex that the
     * token should accept. For example, if you have a Route with a url like
     * `"files/:fileName"` and you want it to match urls like "files/someImage.jpg" then
     * you can set these conditions to allow the :fileName token to accept strings
     * containing a period ("."):
     *
     *     conditions: {
     *         ':fileName': "[0-9a-zA-Z\.]+"
     *     }
     */

    /**
     * @property {String} [defaultMatcher='([%a-zA-Z0-9\\-\\_\\s,]+)'] The default RegExp string
     * to use to match parameters with.
     */
    defaultMatcher: '([%a-zA-Z0-9\\-\\_\\s,]+)',

    /**
     * @private
     * @property {RegExp} matcherRegex A regular expression to match the token to the configured {@link #url}.
     */
    /**
     * @private
     * @property {RegExp} paramMatchingRegex A regular expression to check if there are parameters in the configured {@link #url}.
     */
    paramMatchingRegex: /:([0-9A-Za-z\_]*)/g,
    /**
     * @private
     * @property {Array} paramsInMatchString An array of parameters in the configured {@link #url}.
     */

    /**
     * @protected
     * @property {Boolean} [isRoute=true]
     */
    isRoute: true,

    constructor: function (config) {
        var me = this,
            url;

        this.initConfig(config);

        Ext.apply(me, config, {
            conditions: {}
        });

        url = me.getUrl();

        me.paramsInMatchString = url.match(me.paramMatchingRegex) || [];
        me.matcherRegex = me.createMatcherRegex(url);
    },

    /**
     * Attempts to recognize a given url string and return a meta data object including
     * any URL parameter matches.
     *
     * @param {String} url The url to recognize.
     * @return {Object/Boolean} The matched data, or `false` if no match.
     */
    recognize: function (url) {
        var me = this,
            recognized = me.recognizes(url),
            matches, urlParams;

        if (url === me.lastToken) {
            //url matched the lastToken
            return true;
        }

        if (recognized) {
            matches = me.matchesFor(url);
            urlParams = url.match(me.matcherRegex);

            urlParams.shift();

            return Ext.applyIf(matches, {
                historyUrl: url,
                urlParams: urlParams
            });
        }

        return false;
    },

    /**
     * Returns true if this {@link Ext.route.Route} matches the given url string.
     *
     * @param {String} url The url to test.
     * @return {Boolean} `true` if this {@link Ext.route.Route} recognizes the url.
     */
    recognizes: function (url) {
        return this.matcherRegex.test(url);
    },

    /**
     * The method to execute the action using the configured before function which will
     * kick off the actual {@link #actions} on the {@link #controller}.
     *
     * @param token
     * @param {Object} argConfig The object from the {@link Ext.route.Route}'s
     * recognize method call.
     * @return {Ext.promise.Promise}
     */
    execute: function (token, argConfig) {
        var me = this,
            allowInactive = me.getAllowInactive(),
            handlers = me.getHandlers(),
            queue = Ext.route.Router.getQueueRoutes(),
            length = handlers.length,
            befores = [],
            actions = [],
            urlParams = (argConfig && argConfig.urlParams) || [],
            i, handler, scope, action, promises;

        me.lastToken = token;

        if (!queue) {
            promises = [];
        }

        return new Ext.Promise(function (resolve, reject) {
            for (i = 0; i < length; i++) {
                handler = handlers[i];
                scope = handler.scope;

                if (!allowInactive && scope.isActive && !scope.isActive()) {
                    continue;
                }

                if (queue) {
                    if (handler.before) {
                        befores.push({
                            fn: handler.before,
                            scope: scope
                        });
                    }

                    if (handler.action) {
                        actions.push({
                            fn: handler.action,
                            scope: scope
                        });
                    }
                } else {
                    action = {
                        urlParams: urlParams
                    };

                    if (handler.before) {
                        action.befores = {
                            fn: handler.before,
                            scope: scope
                        };
                    }

                    if (handler.action) {
                        action.actions = {
                            fn: handler.action,
                            scope: scope
                        };
                    }

                    action = new Ext.route.Action(action);

                    if (Ext.fireEvent('beforeroute', action, me) === false) {
                        action.destroy();
                    } else {
                        promises.push(action.run());
                    }
                }
            }

            if (queue) {
                action = new Ext.route.Action({
                    actions: actions,
                    befores: befores,
                    urlParams: urlParams
                });

                if (Ext.fireEvent('beforeroute', action, me) === false) {
                    action.destroy();

                    reject();
                } else {
                    action.run().then(resolve, reject);
                }
            } else {
                Ext.Promise.all(promises).then(resolve, reject);
            }
        });
    },

    /**
     * Returns a hash of matching url segments for the given url.
     *
     * @param {String} url The url to extract matches for
     * @return {Object} matching url segments
     */
    matchesFor: function (url) {
        var params = {},
            keys = this.paramsInMatchString,
            values = url.match(this.matcherRegex),
            length = keys.length,
            i;

        //first value is the entire match so reject
        values.shift();

        for (i = 0; i < length; i++) {
            params[keys[i].replace(':', '')] = values[i];
        }

        return params;
    },

    /**
     * Takes the configured url string including wildcards and returns a regex that can be
     * used to match against a url.
     *
     * @param {String} url The url string.
     * @return {RegExp} The matcher regex.
     */
    createMatcherRegex: function (url) {
        // Converts a route string into an array of symbols starting with a colon. e.g.
        // ":controller/:action/:id" => [':controller', ':action', ':id']
        var paramsInMatchString = this.paramsInMatchString,
            conditions = this.conditions,
            defaultMatcher = this.defaultMatcher,
            length = paramsInMatchString.length,
            modifiers = this.getCaseInsensitive() ? 'i' : '',
            i, params, matcher;

        if (url === '*') {
            // handle wildcard routes, won't have conditions
            url = url.replace('*', '\\*');
        } else {
            for (i = 0; i < length; i++) {
                params = paramsInMatchString[i];
                matcher = conditions[params] || defaultMatcher;
                url = url.replace(new RegExp(params), matcher);
            }
        }

        //we want to match the whole string, so include the anchors
        return new RegExp('^' + url + '$', modifiers);
    },

    /*
     * Adds a handler to the {@link #handlers} stack.
     *
     * @param {Object} handler An object to describe the handler. A handler should define a `fn`
     * and `scope`. If the `fn` is a String, the function will be resolved from the `scope`.
     * @return {Ext.route.Route} this
     */
    addHandler: function (handler) {
        var handlers = this.getHandlers();

        handlers.push(handler);

        return this;
    },

    /**
     * Removes a handler from th {@link #handlers} stack. This normally happens when
     * destroying a class instance.
     *
     * @param {Object/Ext.Base} scope The class instance to match handlers with.
     * @param {Object} config An optional object to describe which handlers to remove.
     * This can be used to remove all handlers with a certain `before` or `action`.
     * @return {Ext.route.Route} this
     */
    removeHandler: function (scope, config) {
        var handlers = this.getHandlers(),
            length = handlers.length,
            newHandlers = [],
            i, handler;

        for (i = 0; i < length; i++) {
            handler = handlers[i];

            if (handler.scope === scope) {
                if (!config ||
                    (
                        Ext.isDefined(config.action) ? handler.action === config.action : true &&
                        Ext.isDefined(config.before) ? handler.before === config.before : true
                    )
                ) {
                    continue;
                }
            }

            newHandlers.push(handler);
        }

        this.setHandlers(newHandlers);

        return this;
    }
});
