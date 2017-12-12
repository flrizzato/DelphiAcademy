/**
 * A `fieldpanel` is a convenient way to manage and load {@link Ext.field.Field fields}.
 * This class does not provide the `form` submit capabilities of
 * {@link Ext.form.Panel formpanel} but is instead designed to be used where data will be
 * saved to a server in other ways (see below) or perhaps as a child of a `formpanel`.
 *
 * Usually a `fieldpanel` just contains a set of fields to display such as the following:
 *
 *      @example
 *      var panel = Ext.create({
 *          xtype: 'fieldpanel',
 *          fullscreen: true,
 *
 *          items: [{
 *              xtype: 'textfield',
 *              name: 'name',
 *              label: 'Name'
 *          }, {
 *              xtype: 'emailfield',
 *              name: 'email',
 *              label: 'Email'
 *          }, {
 *              xtype: 'passwordfield',
 *              name: 'password',
 *              label: 'Password'
 *          }]
 *      });
 *
 * Here we just created a simple container which could be used as a registration form to
 * sign up to your service. We added a plain {@link Ext.field.Text text field} for the
 * user's Name, an {@link Ext.field.Email email field} and finally a
 * {@link Ext.field.Password password field}.
 *
 * In each case we provided a {@link Ext.field.Field#name name} config on the field so
 * that we can identify it later and act on the whole group of fields.
 *
 * ## Gathering Field Data
 *
 * One simple way to get the data from a `fieldpanel` is {@link #getValues}:
 *
 *      var values = panel.getValues();
 *
 *      // values now looks like this:
 *
 *      {
 *          name: 'Peter',
 *          email: 'peter.venkman@gb.com',
 *          password: '**********'
 *      }
 *
 * Or if you have a {@link Ext.data.Model record}, you can use `fillRecord`:
 *
 *      panel.fillRecord(rec);
 *
 * This method is equivalent to the classic toolkit `updateRecord` method, but that name
 * is not used in the modern toolkit due to conflicts with the `record` config property.
 *
 * ## Observing Fields
 *
 * Typical applications use a {@link Ext.app.ViewController controller} to manage events
 * from containers like this:
 *
 *      var panel = Ext.create({
 *          xtype: 'fieldpanel',
 *          fullscreen: true,
 *
 *          controller: 'mycontroller',
 *
 *          items: [{
 *              xtype: 'textfield',
 *              name: 'name',
 *              label: 'Name'
 *          }, {
 *              xtype: 'emailfield',
 *              name: 'email',
 *              label: 'Email'
 *          }, {
 *              xtype: 'passwordfield',
 *              name: 'password',
 *              label: 'Password'
 *          }]
 *      });
 *
 *      Ext.define('MyController', {
 *          extend: 'Ext.app.ViewController',
 *          alias: 'controller.mycontroller',
 *
 *          control: {
 *              '> field': {
 *                  change: 'onChange'
 *              }
 *          },
 *
 *          onChange: function (field, value) {
 *              console.log('change', field.name, value);
 *          }
 *      });
 *
 * The above code responds to a {@link Ext.field.Text#change change} event from any `field`
 * that is an immediate child of its view, the `fieldpanel`.
 *
 * ## Saving Data
 *
 * With a `fieldpanel` you can use the {@link Ext.data.Model#method!save save method} on a
 * record to save data to the server.
 *
 *      var panel = Ext.create({
 *          xtype: 'fieldpanel',
 *          fullscreen: true,
 *
 *          controller: 'mycontroller',
 *
 *          buttons: {
 *              save: 'onSave'
 *          },
 *
 *          items: [{
 *              xtype: 'textfield',
 *              name: 'name',
 *              label: 'Name'
 *          }, {
 *              xtype: 'emailfield',
 *              name: 'email',
 *              label: 'Email'
 *          }, {
 *              xtype: 'passwordfield',
 *              name: 'password',
 *              label: 'Password'
 *          }]
 *      });
 *
 *      Ext.define('MyController', {
 *          extend: 'Ext.app.ViewController',
 *          alias: 'controller.mycontroller',
 *
 *          onSave: function () {
 *              var rec = new MyModel();
 *
 *              this.getView().fillRecord(rec);
 *
 *              rec.save({
 *                  // options
 *              });
 *          }
 *      });
 *
 * To use `form` submit, use {@link Ext.form.Panel formpanel} instead.
 *
 * @since 6.5.0
 */
Ext.define('Ext.field.Panel', {
    extend: 'Ext.Panel',
    xtype: 'fieldpanel',

    mixins: [
        'Ext.field.Manager',
        'Ext.form.Borders'
    ],

    // classCls: Ext.baseCSSPrefix + 'formpanel',

    /**
     * @cfg scrollable
     * @inheritdoc
     */
    scrollable: true,

    /**
     * @cfg nameable
     * @inheritdoc
     * Forms can be assigned names to be used in parent forms.
     */
    nameable: true,

    /**
     * @cfg shareableName
     * @inheritdoc
     * Forms can be assigned the same name as other forms in their parent form. This
     * means that if a form is assigned a `name` it will be returned as an array from
     * `lookupName` in its parent form.
     */
    shareableName: true,

    /**
     * @cfg nameHolder
     * @inheritdoc
     */
    nameHolder: true,

    /**
     * @event exception
     * Fires when either the Ajax HTTP request reports a failure OR the server returns a
     * `success:false` response in the result payload.
     * @param {Ext.field.Panel} this This container.
     * @param {Object} result Either a failed `Ext.data.Connection` request object or a
     * failed (logical) server response payload.
     */

    config: {
        /**
         * @cfg {Object} api
         * If specified, load and submit (see `formpanel`) actions will be loaded and
         * submitted via Ext Direct. Methods which have been imported by
         * {@link Ext.direct.Manager} can be specified here to load and submit forms.
         *
         * API methods may also be specified as strings and will be parsed into the actual
         * functions when the first submit or load has occurred.
         *
         * For example, instead of the following:
         *
         *      api: {
         *          load: App.ss.MyProfile.load,
         *          submit: App.ss.MyProfile.submit
         *      }
         *
         * You can use strings:
         *
         *      api: {
         *          load: 'App.ss.MyProfile.load',
         *          submit: 'App.ss.MyProfile.submit'
         *      }
         *
         * You can also use a prefix instead of fully qualified function names:
         *
         *      api: {
         *          prefix: 'App.ss.MyProfile',
         *          load: 'load',
         *          submit: 'submit'
         *      }
         *
         * Load actions can use {@link #paramOrder} or {@link #paramsAsHash} to customize
         * how the {@link #method!load load method} is invoked.
         *
         * For `formpanel`, submit actions will always use a standard form submit. The
         * `formHandler` configuration (see Ext.direct.RemotingProvider#action) must be
         * set on the associated server-side method which has been imported by
         * {@link Ext.direct.Manager}.
         */
        api: null,

        /**
         * @cfg {Object} baseParams
         * Optional set of params to be sent.
         *
         * For `formpanel` this only applies when `standardSubmit` is set to `false`.
         */
        baseParams: null,

        /**
         * @cfg {String/String[]} paramOrder
         * A list of params to be executed server side. Only used for the
         * {@link #cfg!api load} config.
         *
         * Specify the params in the order in which they must be executed on the
         * server-side as either (1) a String[], or (2) a String of params delimited by
         * either whitespace, comma, or pipe.
         *
         * For example, any of the following would be acceptable:
         *
         *     paramOrder: ['param1','param2','param3']
         *     paramOrder: 'param1 param2 param3'
         *     paramOrder: 'param1,param2,param3'
         *     paramOrder: 'param1|param2|param'
         */
        paramOrder: null,

        /**
         * @cfg {Boolean} paramsAsHash
         * If true, parameters will be sent as a single hash collection of named arguments.
         * Providing a {@link #paramOrder} nullifies this configuration.
         *
         * Only used for the {@link #cfg!api load} config.
         */
        paramsAsHash: null,

        /**
         * @cfg {Number} timeout
         * Timeout for server actions (in seconds).
         */
        timeout: 30,

        /**
         * @cfg {String} url
         * The default URL for server actions (`load` and `submit` in `formpanel`).
         */
        url: null
    },

    /**
     * Performs an Ajax or Ext Direct call to load values for this form.
     *
     * @param {Object} options
     * The configuration when loading this form.
     *
     * The following are the configurations when loading via Ajax only:
     *
     * @param {String} options.url
     * The url for the action (defaults to the form's {@link #url}).
     *
     * @param {String} options.method
     * The form method to use (defaults to the form's {@link #method}, or GET if not
     * defined).
     *
     * @param {Object} options.headers
     * Request headers to set for the action.
     *
     * @param {Number} options.timeout
     * The number is seconds the loading will timeout in.
     *
     * The following are the configurations when loading via Ajax or Direct:
     *
     * @param {Boolean} [options.autoAbort=false]
     * `true` to abort any pending Ajax request prior to loading.
     *
     * @param {String/Object} options.params
     * The params to pass when submitting this form (defaults to {@link #baseParams}).
     * Parameters are encoded as standard HTTP parameters using {@link Ext#urlEncode}.
     *
     * @param {String/Object} [options.waitMsg]
     * If specified, the value which is passed to the loading {@link #masked mask}. See
     * {@link #masked} for more information.
     *
     * @param {Function} options.success
     * The callback that will be invoked after a successful response. A response is
     * successful if a response is received from the server and is a JSON object where the
     * `success` property is set to `true`, `{"success": true}`.
     *
     * The function is passed the following parameters and can be used for loading via
     * Ajax or Direct:
     *
     * @param {Ext.form.Panel} options.success.form
     * The {@link Ext.form.Panel} that requested the load.
     *
     * @param {Object/Ext.direct.Event} options.success.result
     * The result object returned by the server as a result of the load request. If the
     * loading was done via Ext Direct, will return the {@link Ext.direct.Event} instance,
     * otherwise will return an Object.
     *
     * @param {Object} options.success.data
     * The parsed data returned by the server.
     *
     * @param {Function} options.failure
     * The callback that will be invoked after a failed transaction attempt.
     *
     * The function is passed the following parameters and can be used for loading via
     * Ajax or Direct:
     *
     * @param {Ext.form.Panel} options.failure.form
     * The {@link Ext.form.Panel} that requested the load.
     *
     * @param {Ext.form.Panel} options.failure.result
     * The failed response or result object returned by the server which performed the
     * operation.
     *
     * @param {Object} options.scope
     * The scope in which to call the callback functions (The `this` reference for the
     * callback functions).
     *
     * @return {Ext.data.Connection} The request object.
     */
    load: function (options) {
        options = options || {};

        var me = this,
            api = me.getApi(),
            url = options.url || me.getUrl(),
            waitMsg = options.waitMsg,
            successFn = function (response, data) {
                me.setValues(data.data);

                if (Ext.isFunction(options.success)) {
                    options.success.call(options.scope || me, me, response, data);
                }

                me.fireEvent('load', me, response);
            },
            failureFn = function (response, data) {
                if (Ext.isFunction(options.failure)) {
                    options.failure.call(options.scope, me, response, data);
                }

                me.fireEvent('exception', me, response);
            },
            load, args;

        if (options.waitMsg) {
            if (typeof waitMsg === 'string') {
                waitMsg = {
                    xtype   : 'loadmask',
                    message : waitMsg
                };
            }

            me.setMasked(waitMsg);
        }

        if (api) {
            api = Ext.direct.Manager.resolveApi(api, me);
            me.setApi(api);

            load = api.load;

            if (!load) {
                Ext.raise("Cannot find Ext Direct API method for load action");
            }

            args = load.$directCfg.method.getArgs({
                params: me.getParams(options.params),
                paramOrder: me.getParamOrder(),
                paramsAsHash: me.getParamsAsHash(),
                scope: me,
                callback: function (data, response, success) {
                    me.setMasked(false);

                    if (success) {
                        successFn(response, data);
                    } else {
                        failureFn(response, data);
                    }
                }
            });

            load.apply(window, args);
        }
        else if (url) {
            return Ext.Ajax.request({
                url: url,
                timeout: (options.timeout || me.getTimeout()) * 1000,
                method: options.method || 'GET',
                autoAbort: options.autoAbort,
                headers: Ext.apply(
                    {
                        'Content-Type' : 'application/x-www-form-urlencoded; charset=UTF-8'
                    },
                    options.headers || {}
                ),
                callback: function (callbackOptions, success, response) {
                    var responseText = response.responseText,
                        statusResult = Ext.data.request.Ajax.parseStatus(response.status, response);

                    me.setMasked(false);

                    if (success) {
                        if (statusResult && responseText.length === 0) {
                            success = true;
                        } else {
                            response = Ext.decode(responseText);
                            success = !!response.success;
                        }

                        if (success) {
                            successFn(response, responseText);
                        } else {
                            failureFn(response, responseText);
                        }
                    }
                    else {
                        failureFn(response, responseText);
                    }
                }
            });
        }
    },

    /**
     * @private
     */
    getParams: function (params) {
        return Ext.apply({}, params, this.getBaseParams());
    },

    updateDisabled: function (newDisabled, oldDisabled) {
        this.mixins.fieldmanager.updateDisabled.call(this, newDisabled, oldDisabled);

        this.callParent([newDisabled, oldDisabled]);
    },

    updateRecord: function(record) {
        this.consumeRecord(record);
    }
});
