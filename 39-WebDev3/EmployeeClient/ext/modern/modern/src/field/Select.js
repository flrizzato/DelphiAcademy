/**
 * Simple Select field wrapper. Example usage:
 *
 *     @example
 *     Ext.create('Ext.form.Panel', {
 *         fullscreen: true,
 *         items: [{
 *             xtype: 'fieldset',
 *             title: 'Select',
 *             items: [{
 *                 xtype: 'selectfield',
 *                 label: 'Choose one',
 *                 options: [{
 *                     text: 'First Option',
 *                     value: 'first'
 *                 }, {
 *                     text: 'Second Option',
 *                     value: 'second'
 *                 }, {
 *                     text: 'Third Option',
 *                     value: 'third'
 *                 }]
 *             }]
 *         }]
 *     });
 */
Ext.define('Ext.field.Select', {
    extend: 'Ext.field.Picker',
    xtype: 'selectfield',

    alternateClassName: 'Ext.form.Select',

    requires: [
        'Ext.Panel',
        'Ext.picker.Picker',
        'Ext.picker.Tablet',
        'Ext.data.Store',
        'Ext.data.StoreManager',
        'Ext.dataview.BoundList'
    ],

    /**
     * @property {Boolean} isSelectField
     * `true` to identify an object as an instance of this class, or a subclass thereof.
     */
    isSelectField: true,

    /**
     * @event change
     * Fires when selection has changed.
     *
     * This includes keystrokes that edit the text (if editable).
     * @param {Ext.field.Select} this
     * @param {Ext.data.Model} newValue The corresponding record for the new value
     * @param {Ext.data.Model} oldValue The corresponding record for the old value
     */

    /**
     * @event select
     * Fires when an option from the drop down list has been selected.
     * @param {Ext.field.Select} this
     * @param {Ext.data.Model} newValue The corresponding record for the new value
     */

    /**
     * @event focus
     * Fires when this field receives input focus. This happens both when you tap on the
     * field and when you focus on the field by using 'next' or 'tab' on a keyboard.
     *
     * Please note that this event is not very reliable on Android. For example, if your
     * Select field is second in your form panel, you cannot use the Next button to get to
     * this select field. This functionality works as expected on iOS.
     * @param {Ext.field.Select} this This field
     * @param {Ext.event.Event} e
     */

    config: {
        /**
         * @cfg {Object|Ext.util.Collection} valueCollection
         * A {@link Ext.util.Collection collection} instance, or configuration object used
         * to create the collection of selected records.
         *
         * This is used by the {@link #cfg!picker} as the core of its selection handling,
         * and also as the collection of selected values for this widget.
         *
         * @readonly
         * @private
         * @since 6.5.0
         */
        valueCollection: true,

        /**
         * @cfg {String/Number} valueField
         * The underlying {@link Ext.data.Field#name data value name} to bind to this
         * Select control. If configured as `null`, the {@link #cfg!displayField} is
         * used.
         * @accessor
         */
        valueField: 'value',

        /**
         * @cfg {String/Ext.XTemplate} itemTpl
         * An XTemplate definition string (Or an {@link Ext.XTemplate}) which specifies
         * how to display a list item from a record values object. This is automatically
         * generated to display the {@link #cfg!displayField} if not specified.
         */
        itemTpl: false,

        /**
         * @cfg {String/String[]/Ext.XTemplate} displayTpl
         * The template to be used to display the selected record inside the text field.
         *
         * If not specified, the {@link #cfg!displayField} is shown in the text field.
         */
        displayTpl: null,

        /**
         * @cfg {String/Number} displayField
         * The underlying {@link Ext.data.Field#name data value name} to bind to this
         * Select control.  If configured as `null`, the {@link #cfg!valueField} is used.
         *
         * This resolved value is the visibly rendered value of the available selection
         * options.
         * @accessor
         */
        displayField: 'text',

        /**
         * @cfg {Ext.data.Store/Object/String} store
         * The store to provide selection options data. Either a Store instance,
         * configuration object or store ID.
         * @accessor
         */
        store: null,

        /**
         * @cfg {Array} options
         * An array of select options.
         *
         *     [
         *         {text: 'First Option',  value: 'first'},
         *         {text: 'Second Option', value: 'second'},
         *         {text: 'Third Option',  value: 'third'}
         *     ]
         *
         * __Note:__ Option object member names should correspond with defined
         * {@link #valueField valueField} and {@link #displayField displayField} values.
         *
         * This config is mutually exclusive with the {@link #cfg!store} config. Specifying
         * them both is unssupported and will produce undefined behaviour.
         * @accessor
         */
        options: null,

        /**
         * @cfg {String} hiddenName
         * Specify a `hiddenName` if you're using the {@link Ext.form.Panel#standardSubmit}
         * option. This name will be used to post the underlying value of the select to
         * the server.
         * @accessor
         */
        hiddenName: null,

        /**
         * @cfg {Boolean/'initial'} autoSelect
         * `true` to auto select the first value in the {@link #store} or {@link #options}
         * when they are changed. This settings attempts to avoid the {@link #value} being
         * set to `null`, unless {@link #clearable clearable} is also `true` in which case
         * only other changes (such as store load) will trigger auto-selection.
         *
         * If this value is `'initial'` then auto selection will only occur on the first
         * opportunity (such as initial store load). This config will then be set to
         * `false`.
         */
        autoSelect: false,

        /**
         * @cfg {Boolean} autoFocus
         * `true` to automatically focus the first result gathered by the data store in the
         * dropdown list when it is opened. A false value would cause nothing in the list
         * to be highlighted automatically, so the user would have to manually highlight an
         * item before pressing the enter or {@link #selectOnTab tab} key to select it
         * (unless the value of ({@link #typeAhead}) were true), or use the mouse to select
         * a value.
         */
        autoFocus: true,

        /**
         * @cfg {Boolean} autoFocusLast
         * When `true`, the last selected record in the dropdown list will be re-selected
         * upon {@link #autoFocus}. Set to `false` to always select the first record in
         * the drop-down list. For accessible applications it is recommended to set this
         * option to `false`.
         */
        autoFocusLast: true,

        /**
         * @cfg {Ext.data.Model} selection
         * @accessor
         * The selected model. `null` if no value exists.
         */
        selection: null,

        /**
         * @cfg {Boolean} autoLoadOnValue
         * This option controls whether to initially load the store when a value is set so
         * that the display value can be determined from the appropriate record.
         *
         * The store will only be loaded in a limited set of circumstances:
         * - The store is not currently loading.
         * - The store does not have a pending {@link Ext.data.Store#autoLoad}.
         * - The store has not been loaded before.
         */
        autoLoadOnValue: false,

        /**
         * @cfg {Boolean} forceSelection
         * By default the value must always be the {@link #cfg!valueField} of one of the
         * records in the store. Configure as `false` to allow the value to be set to
         * arbitrary text, and have this component auto create an associated record with
         * the typed value as the {@link #cfg!valueField}.
         *
         * This config is only supported for use in {@link Ext.field.ComboBox} but is defined
         * here (as private) because of its many entanglements with value processing.
         * @private
         * @since 6.5.0
         */
        forceSelection: true,

        /**
         * @cfg {String} valueNotFoundText
         * The message to display if the value passed to `setValue` is not found in the store.
         */
        valueNotFoundText: null,

        /**
         * @cfg {Boolean} selectOnTab
         * Whether the Tab key should select the currently highlighted item.
         */
        selectOnTab: true
    },

    /**
     * @cfg editable
     * @inheritdoc
     */
    editable: false,

    /**
     * @cfg floatedPicker
     * @inheritdoc
     */
    floatedPicker: {
        xtype: 'boundlist',
        infinite: false,
        // BoundListNavigationModel binds to input field
        // Must only be enabled when list is visible
        navigationModel: {
            disabled: true
        },
        scrollToTopOnRefresh: false,
        loadingHeight: 70,
        maxHeight: 300,
        floated: true,
        axisLock: true,
        hideAnimation: null
    },

    /**
     * @cfg edgePicker
     * @inheritdoc
     */
    edgePicker: {
        xtype: 'picker',
        cover: true
    },

    /**
     * @property classCls
     * @inheritdoc
     */
    classCls: Ext.baseCSSPrefix + 'selectfield',

    /**
     * @cfg twoWayBindable
     * @inheritdoc
     */
    twoWayBindable: {
        selection: 1
    },

    /**
     * @cfg publishes
     * @inheritdoc
     */
    publishes: {
        selection: 1
    },

    applyValueCollection: function(valueCollection) {
        if (!valueCollection.isCollection) {
            valueCollection = new Ext.util.Collection(valueCollection);
        }

        // Add this SelectField as an observer immediately so that we are informed of any
        // mutations which occur in this event run.
        // We must sync the selection property and the rawValue upon mutation.
        valueCollection.addObserver(this);

        return valueCollection;
    },

    /**
     * This method is called to create a temporary record when the value entered does not
     * match a record in the `store` (when {@link #cfg!forceSelection} is `false`).
     *
     * The `data` object passed contains the typed value in both the {@link #cfg!valueField}
     * and the {@link #cfg!displayField}.
     *
     * The record created and returned from this method will be the {@link #cfg!selection}
     * value in this non-matching state.
     *
     * @param data The data object used to create the new record.
     * @return {Ext.data.Model} The new record.
     * @template
     * @since 6.5.1
     */
    createSelectionRecord: function (data) {
        var Model = this.getStore().getModel();

        return new Model(data);
    },

    completeEdit: Ext.emptyFn,

    expand: function() {
        // If we do not yet have a store (binding not arrived yet), we cannot expand.
        if (this.getStore()) {
            this.callParent();
        }
    },

    /**
     * @private
     */
    maybeCollapse: function(event) {
        var record = event.to && event.to.record,
            selection = this.getSelection();

        if (record === selection) {
            this.collapse();
        }
    },

    /**
     * @private
     * Respond to deselection. Call the onItemDeselect template method
     */
    onCollectionRemove: function(valueCollection, chunk) {
        var selection = valueCollection.getRange();

        // If this remove is part of a splice, wait until the collection add to sync the selection.
        if (!chunk.replacement) {
            // Must ensure that null is passed if the valueCollection is empty
            this.setSelection(selection.length ? selection[0] : null);
        }
    },

    /**
     * @private
     * Respond to selection. Call the onItemSelect template method
     */
    onCollectionAdd: function(valueCollection, adds) {
        var selection = valueCollection.getRange();

        this.setSelection(selection[0]);
    },

    clearValue: function () {
        var me = this;

        // We clear things differently vs superclass. The value of Select fields depends
        // upon the value collection.
        me.forceInputChange = true;
        me.setValue(null);
        me.forceInputChange = false;

        me.syncEmptyState();
    },

    /* TODO fixup these docs and move to value config
     * Sets the value of the field.
     * @param {Mixed/Ext.data.Model} newValue The new value. This may be specified as either
     * an existing store record, or the required {@link #cfg!valueField} value.
     *
     * Either way, both {@link #cfg!valueField} value *and* the associated record will be ascertained.
     *
     * The {@link #cfg!valueField} value is published to the ViewModel as is the {@link #cfg-selection associated record}.
     *
     * The record published to the selection property will be `null` if the value did not
     * match a record, and the field is not configured to create new records for unmatched
     * values using `{@link #cfg!forceSelection}: false`
     */

    applyValue: function(value, oldValue) {
        // Ensure that a store is formed from any options before we get the store.
        this.getOptions();

        var me = this,
            store = me.getStore();

        // syncValue must now prioritize the value over the inputValue
        me.syncMode = 'value';

        // We were passed a record.
        // Set the selection which updates the value from the valueField.
        if (value && value.isEntity) {
            me.setSelection(value);
            return;
        }

        if (me.isConfiguring) {
            me.originalValue = value;
        }

        // Kick off a load unless we are clearing the value.
        // Doesn't matter whether proxy is remote - it needs loading
        // so we can select the correct record for the value in the load event handler.
        if (store && value) {
            // If we are configured to autoLoad when the value arrives, prepare to do so
            if (me.getAutoLoadOnValue() && !store.isLoaded() && !store.hasPendingLoad()) {
                store.load();
            }
        }

        return me.transformValue(value);
    },

    updateValue: function(value, oldValue) {
        this.syncValue();

        // Note that we must not invoke superclass updateValue because that updates the
        // field UI in ways that SelectFields cannot handle.
        // We must directly invoke the base class's updateValue. That fires the change
        // event and validates the value which we still need to happen.
        Ext.field.Field.prototype.updateValue.call(this, value, oldValue);
    },

    transformValue: function (value) {
        if (value == null || value === '') {
            value = this.getForceSelection() ? null : '';
        }

        return value;
    },

    /**
     * Finds the record in the {@link #cfg!store}, or the {@link #cfg!valueCollection} which has the {@link #cfg!valueField}
     * matching the passed value.
     *
     * The {@link #cfg!valueCollection} is included because of the {@link #cfg!createNewOnEnter},
     * {@link #cfg!createNewOnBlur}, and {@link #cfg!forceSelection} configs which allow for insertion into the
     * {@link #cfg!valueCollection} of newly created records which are not in the configured {@link #cfg!store}.
     *
     * Also, a currently selected value may be filtered out of visibility in the configured {@link #cfg!store}
     *
     * @param {String} value The value to match the {@link #valueField} against.
     * @return {Ext.data.Model} The matched record or null.
     */
    findRecordByValue: function(value) {
        var me = this,
            store = me.getStore(),
            valueField = me.getValueField(),
            result,
            ret = null;

        if (store) {
            result = store.byValue.get(value);

            // If there are duplicate keys, tested behaviour is to return the *first* match.
            if (result) {
                ret = result[0] || result;
            }
        }

        // Not found in the base store.
        // See if there's a match in the valueCollection.
        // This is because we allow new records to be created if forceSelection is false
        // And we allow value to be set to a record which is then inserted into the valueCollection.
        if (!ret) {
            ret = me.getValueCollection().findBy(function(record) {
                return record.get(valueField) === value;
            });
        }
        return ret;
    },

    /**
     * Finds the record by searching values in the {@link #displayField}.
     * @param {Object} value The value to match the field against.
     * @return {Ext.data.Model/false} The matched record or `false`.
     */
    findRecordByDisplay: function(value) {
        var store = this.getStore(),
            result,
            ret = false;

        if (store) {
            result = store.byText.get(value);
            // If there are duplicate keys, tested behaviour is to return the *first* match.
            if (result) {
                ret = result[0] || result;
            }
        }
        return ret;
    },

    /**
     * @private
     * Update the UI to reflect the new selection. The selection arrives as mutation notifications
     * from the {@link #cfg!valueCollection} which is the {@link Ext.util.Collection} at the heart
     * of the picker's {@link Ext.mixin.Selectable} persona.
     */
    updateSelection: function(selection, oldSelection) {
        var me = this,
            isNull = selection == null,
            valueCollection = me.getValueCollection(),
            valueField = me.getValueField(),
            oldValue = me._value,
            newValue = null,
            picker, spliceArgs;

        if (me._ignoreSelection || me.destroyed || me.destroying) {
            return;
        }

        if (isNull || !valueCollection.containsAll(selection)) {
            spliceArgs = [0, valueCollection.getCount()];

            // If the selection isNull, do not append the final "toAdd" argument.
            // That would attempt to add null which would throw an error.
            if (!isNull) {
                spliceArgs.push(selection);
            }

            // Replace all valueCollection content with the new selection.
            // We are an observer of the valueCollection.
            //
            // This will feed through to our onCollectionRemove, which will only
            // push through to the selection property if there's no upcoming add.
            //
            // If there's an add, then our onCollectionAdd will be called
            // which will push the valueCollection's data through to
            // our selection property.
            valueCollection.splice.apply(valueCollection, spliceArgs);

            // In case splice user event handler destroyed us.
            if (me.destroyed) {
                return;
            }
        }

        if (selection) {
            if (valueField) {
                newValue = selection.get(valueField);
                me.setValue(newValue);
            }

            // Allow selection to be vetoed, in which case fall back to oldValue
            if (me.fireEvent('select', me, selection) === false) {
                me.setValue(oldValue);
                me._selection = oldSelection;
            }
        }
        else {
            me.clearValue();
        }

        // Event handlers may destroy this component
        if (me.destroyed) {
            return;
        }

        // Update the field's input UI.
        // Note that this may be a DOM <input> value, but may also
        // be a UI like a TagField which is produced from the
        // selected record(s)
        me.setFieldDisplay(selection);

        // Only get the picker if it has been created.
        picker = me.getConfig('picker', false, true);

        // If the picker has been created, either collapse it,
        // or scroll to the latest selection.
        if (picker && picker.isVisible()) {
            // The setter's equality test cannot tell if the single selected record
            // is in effect unchanged. We only need to collapse if a *new* value has
            // been set, that is, the user has selected a record with a different id.
            // We can get here when the selection is refreshed due to record add/remove
            // when the record *instance* is renewed, but it is the same id.
            // In that case, all we need is a refresh of the data in case the record's
            // data payload changed.
            //
            // If unchanged, it's possible that other data in the record may have changed
            // which could affect the BoundList, so refresh that
            if (selection && oldSelection && selection.id === oldSelection.id) {
                picker.refresh();
            } else {
                // If it's a single select, dynamically created record, this is due
                // to typing, so do not collapse.
                if (!(selection && selection.isEntered)) {
                    me.collapse();
                }
            }
        }
    },

    /**
     * Gets data for each record to be used for constructing the display value with
     * the {@link #displayTpl}. This may be overridden to provide access to associated records.
     * @param {Ext.data.Model} record The record.
     * @return {Object} The data to be passed for each record to the {@link #displayTpl}.
     *
     * @protected
     * @template
     */
    getRecordDisplayData: function(record) {
        return record.getData();
    },

    createFloatedPicker: function() {
        var me = this,
            result = Ext.merge({
                ownerCmp: me,
                store: me._pickerStore || me.getStore(),
                selectable: {
                    selected: me.getValueCollection(),
                    selectedRecord: me.getSelection(),
                    mode: 'single'
                },
                itemTpl: me.getItemTpl()
            }, me.getFloatedPicker());

        // Allow SPACE to navigate unless it's needed
        // to edit the inputElement.
        result.navigationModel.navigateOnSpace = !me.getEditable();

        return result;
    },

    createEdgePicker: function() {
        var me = this;

        return Ext.merge({
            ownerCmp: me,
            slots: [{
                align: me.getPickerSlotAlign(),
                name: me.getValueField(),
                valueField: me.getValueField(),
                displayField: me.getDisplayField(),
                value: me.getValue(),
                store: me._pickerStore || me.getStore()
            }],
            listeners: {
                change: me.onPickerChange,
                scope: me
            },
            setStore: function(store) {
                this.child('pickerslot').setStore(store);
            },
            deselectAll: function() {
                this.child('pickerslot').deselectAll();
            }
        }, me.getEdgePicker());
    },

    setPickerLocation: function(fromKeyboard) {
        var me = this,
            picker = me.getConfig('picker', false, true),
            store, location;

        if (picker && me.expanded) {
            // If an edge picker, access the slot which is a List
            if (picker.isPicker) {
                picker = picker.innerItems[0];
            }
            store = picker.getStore();

            if (picker.getViewItems().length) {
                // If there's a selection, we always move focus to it
                location = picker.getSelectable().getLastSelected();

                // If there's no selection, or the selection is not in the picker store,
                // then autoFocusLast attempts to focus the last known focused location.
                // And the fallback is autoFocus focusing record 0.
                if (!location || !store.contains(location)) {
                    if (fromKeyboard || me.getAutoFocusLast()) {
                        location = picker.getNavigationModel().lastLocation;
                        if (location) {
                            location = location.refresh();
                        }
                    }
                    if (!location && (fromKeyboard || me.getAutoFocus())) {
                        location = store.getAt(0);
                    }
                }

                picker.getNavigationModel().setLocation(location);
            }
        }
    },

    updatePickerValue: function (picker, value) {
        var name = this.getValueField(),
            pickerValue = {};

        if (!value) {
            value = this.getValue();
        }

        pickerValue[name] = value;

        picker.setValue(pickerValue);
    },

    onPickerShow: function(picker) {
        this.callParent([picker]);

        // Enable the picker's key mappings in this field's KeyMap,
        // unless it's an edge picker that doesn't support keyboard
        if (this.pickerType === 'floated') {
            picker.getNavigationModel().enable();
        }
    },

    onPickerHide: function(picker) {
        var navModel;
        
        this.callParent([picker]);

        // Set the location to null because there's no onFocusLeave
        // to do this because the picker does not get focused.
        // Disable the picker's key mappings in this field's KeyMap
        if (!picker.destroying && this.pickerType === 'floated') {
            navModel = picker.getNavigationModel();

            navModel.setLocation(null);
            navModel.disable();
        }
    },

    /**
     * @private
     * Used when the edge picker is used.
     */
    onPickerChange: function(picker, value) {
        this.setValue(this.findRecordByValue(value[this.getValueField()]));
    },

    applyItemTpl: function (itemTpl) {
        if (itemTpl === false) {
            itemTpl = '<span class="x-list-label">{' + this.getDisplayField() + ':htmlEncode}</span>';
        }
        return itemTpl;
    },

    applyDisplayTpl: function (displayTpl) {
        if (displayTpl && !displayTpl.isTemplate) {
            displayTpl = new Ext.XTemplate(displayTpl);
        }
        return displayTpl;
    },

    applyOptions: function(options) {
        if (options) {
            var len = options.length,
                valueField = this.getValueField(),
                displayField = this.getDisplayField(),
                i, value, option;

            // Convert an array of primitives to record data objects
            options = Ext.Array.slice(options);
            for (i = 0; i < len; i++) {
                value = options[i];
                if (Ext.isPrimitive(value)) {
                    options[i] = option = {};
                    option.id = value;
                    option[valueField] = value;
                    if (displayField && displayField !== valueField) {
                        option[displayField] = value;
                    }
                }
            }

            options = Ext.data.StoreManager.lookup({
                fields: [valueField, displayField],
                data: options
            });
        }
        return options;
    },

    updateOptions: function(options, oldOptions) {
        if (options) {
            this.setStore(options);
        } else {
            if (oldOptions === this.getStore()) {
                this.setStore(null);
            }
        }
    },

    applyStore: function(store) {
        if (store) {
            store = Ext.data.StoreManager.lookup(store);
        }

        return store;
    },

    updateStore: function(store, oldStore) {
        var me = this,
            valueField = me.getValueField(),
            displayField = me.getDisplayField(),
            extraKeySpec;

        if (oldStore) {
            if (oldStore.getAutoDestroy()) {
                oldStore.destroy();
            } else {
                oldStore.byValue = oldStore.byText = Ext.destroy(oldStore.byValue, oldStore.byText);
            }
        }

        if (store) {
            // Add a byValue index to the store so that we can efficiently look up records by the value field
            // when setValue passes string value(s).
            // The two indices (Ext.util.CollectionKeys) are configured unique: false, so that if duplicate keys
            // are found, they are all returned by the get call.
            // This is so that findByText and findByValue are able to return the *FIRST* matching value. By default,
            // if unique is true, CollectionKey keeps the *last* matching value.
            extraKeySpec = {
                byValue: {
                    rootProperty: 'data',
                    unique: false,
                    property: valueField
                }
            };
            if (displayField !== valueField) {
                extraKeySpec.byText = {
                    rootProperty: 'data',
                    unique: false,
                    property: displayField
                };
            }
            store.setExtraKeys(extraKeySpec);

            // If display and value fields are the same, the same index goes by both names.
            if (displayField === valueField) {
                store.byText = store.byValue;
            }

            store.on({
                scope: me,
                add: 'onStoreDataChanged',
                remove: 'onStoreDataChanged',
                update: 'onStoreRecordUpdated',

                // Must be informed after list, and selection has been updated
                load: {
                    fn: 'onStoreLoad',
                    priority: -1
                }
            });

            // If the store is already loaded, fix up any value we may have.
            // cachedValue will be set if there was no store at init time.
            // If we had a selected record, rematch it.
            // Otherwise auto select first record if configured to do so.
            if (store.isLoaded() && !store.hasPendingLoad()) {
                me.syncValue();
            }
            // If not loaded, and there's a value waiting to be matched
            // and we should autoload on value, load the store and onStoreLoad
            // will match it up.
            else if (me.getValue() != null && me.getAutoLoadOnValue() && 
                     !store.isLoaded() && !store.hasPendingLoad()) {
                store.load();
            }
        }

        // Depending upon configurations, we may need a ChainedStore to drive
        // the picker.
        me.updatePickerStore();
    },

    applyValueField: function(valueField) {
        // If either valueField or displayField are configured as null, then
        // this Select component uses the remaining configured field name for both purposes.
        if (valueField == null) {
            valueField = this.getDisplayField();
        }
        return valueField;
    },

    updateValueField: function(valueField) {
        var store = this.getStore();

        // Keep the byValue index synced
        if (store && !this.isConfiguring) {
            store.byValue.setCollection(null);
            store.setExtraKeys({
                byValue: {
                    rootProperty: 'data',
                    unique: false,
                    property: valueField
                }
            });
        }
    },

    applyDisplayField: function(displayField) {
        // If either valueField or displayField are configured as null, then
        // this Select component uses the remaining configured field name for both purposes.
        if (displayField == null) {
            displayField = this.getValueField();
        }
        return displayField;
    },

    updateDisplayField: function(displayField) {
        var store = this.getStore();

        // Keep the byValue index synced
        if (store && !this.isConfiguring) {
            store.byText.setCollection(null);
            store.setExtraKeys({
                byText: {
                    rootProperty: 'data',
                    unique: false,
                    property: displayField
                }
            });
        }
    },

    /**
     * @private
     * Whenever the store loads, we need to refresh the selection by pushing a value through
     * the setValue machinery. Upon initialization, there may be a cached initial value.
     * Otherwise use the current value.
     */
    onStoreLoad: function (store, records, success) {
        var filtering = this.isFiltering;
        
        this.isFiltering = false;
        
        if (success) {
            // The isFilering flag is set in doFilter if the store
            // if using remote filters and the primaryFilter has a value.
            this.syncMode = filtering ? 'filter' : 'store';
            this.syncValue();
        }
    },

    syncValue: function() {
        var me = this,
            store = me.getStore(),
            valueField = me.getValueField(),
            displayField = me.getDisplayField(),
            forceSelection = me.getForceSelection(),
            valueNotFoundText = me.getValueNotFoundText(),
            is, isCleared, isInput, value, matchedRecord, dataObj;

        // If we are not ready to reconcile values for any reason.
        //   We are in the middle of value syncing
        //   Store has not arrived from bind
        //   Store has not been loaded
        //   Store is currently loading
        // Then we cannot recconcile values now, this will be called later
        // when the store arrives, or is loaded.
        if (me.reconcilingValue || !store || !store.isLoaded() || store.hasPendingLoad()) {
            return;
        }

        me.reconcilingValue = true;

        me.getSelection(); // make sure selection config is flushed

        is = {};
        is[me.syncMode] = true;
        value = (isInput = is.input || is.filter) ? me.getInputValue() : me.getValue();
        isCleared = value == null || value === '';

        // Get the record that matches our input value
        if (!isCleared) {
            matchedRecord = (isInput ? store.byText : store.byValue).get(value);

            if (matchedRecord) {
                if (!matchedRecord.isEntity) {
                    // Since we lookup values not id's there can be multiple matching
                    // records... so if we get back something that isn't a record, it is
                    // an array.
                    matchedRecord = matchedRecord[0];
                }
            }
            else if (!forceSelection) {
                // Not found in the regular indexes which index the store.
                // If we are potentially inserting unmatched values as new "isEntered"
                // records, then find a match in the valueCollection if possible.
                matchedRecord = me.findRecordByValue(value);
            }
        }

        // Either user has typed something (isInput), or we've had a setValue
        // to a value which has no match in the store, and we are not forceSelection: true.
        // We create a new record.
        if (!isCleared && !matchedRecord && !forceSelection) {
            dataObj = {};
            dataObj[displayField] = value;

            if (valueField && displayField !== valueField) {
                dataObj[valueField] = value;
            }

            matchedRecord = me.createSelectionRecord(dataObj);
            matchedRecord.isEntered = true;
        }
        else {
            // Not in an record.isEntered situation.
            // Value is the typed value.
            if (isInput || is.store) {
                if (!matchedRecord && forceSelection) {
                    me.setValue(null);
                    me.setSelection(null);

                    // If we're processing a store load in response to remote filtering
                    // then we must not clear the input value used to kick off that filter.
                    // If they blur the field now, completeEdit will clear the value as unmatched.
                    if (!is.filter) {
                        me.setFieldDisplay();
                    }
                }
            }
            // Value is the set value.
            else {
                if (isCleared) {
                    if (me.mustAutoSelect()) {
                        matchedRecord = store.first();

                        if (me.getAutoSelect() === 'initial') {
                            me.setAutoSelect(false);
                        }
                    } else {
                        me.setSelection(null);
                    }
                }
                // We have a value, so get the record that matches our current value.
                // Note that setValue can
                else if (!matchedRecord && valueNotFoundText) {
                    me.setError(valueNotFoundText);
                }
            }
        }

        if (matchedRecord) {
            me.setSelection(matchedRecord);
        }

        me.reconcilingValue = false;
    },

    /**
     * @private
     * Called when the internal {@link #store}'s data has changed.
     */
    onStoreDataChanged: function () {
        if (this.getForceSelection()) {
            var value = this.getValue();

            // Push the textual value from the selected record through applyValue
            // to match with a new record from the new data.
            if (value != null) {
                this.setValue(value);
            }
        }
    },

    /**
     * @private
     * Called when a internal {@link #store}'s record has been mutated.
     * Keep the field UI synced
     */
    onStoreRecordUpdated: function(store, record) {
        if (this.getValueCollection().contains(record)) {
            this.updateSelection(this.getSelection());
        }
    },

    /**
     * Resets the Select field to the value of the first record in the store.
     * @return {Ext.field.Select} this
     * @chainable
     */
    reset: function() {
        var me = this,
            picker = me.getConfig('picker', false, true),
            record = me.originalValue || null,
            store;

        if (me.getAutoSelect()) {
            store = me.getStore();
            record = (record != null) ? record : store && store.getAt(0);
        } else {
            if (picker) {
                picker.deselectAll();
            }
        }

        me.setValue(record);
        return me;
    },

    doDestroy: function() {
        var store = this.getStore();

        if (store && !store.destroyed && store.getAutoDestroy()) {
            store.destroy();
        }
        this.destroyMembers('options');

        this.callParent();
    },

    privates: {
        syncMode: null,

        mustAutoSelect: function () {
            var me = this,
                autoSelect = me.getAutoSelect();

            if (autoSelect && !(me.isConfiguring || autoSelect === 'initial')) {
                autoSelect = !me.getClearable() && me.getRequired();
            }

            return !!autoSelect;
        },

        /**
         * Returns ths Store used to drive the BoundList.
         *
         * When the supplied store is `queryMode: 'local'`, this will be a ChainedStore sources from the
         * configured store.
         * @private
         */
        updatePickerStore: function() {
            var me = this,
                picker = me.getConfig('picker', false, true),
                store = me.getStore(),
                localFiltering = me.getQueryMode && me.getQueryMode() === 'local',
                result = store;

            // If we need to be adding local filters, then we need to chain off a store based
            // on the supplied store so that we can own the filtering.
            if (localFiltering) {
                // Already got a ChainedStore - just reconfigure it.
                if (me._pickerStore && me._pickerStore.isChainedStore) {
                    me._pickerStore.setConfig({
                        source: store
                    });
                }
                // Create a ChainedStore based on our store
                else {
                    me._pickerStore = result = Ext.data.StoreManager.lookup({
                        type: 'chained',
                        source: store
                    }, null, me);
                }
            }
            // The _pickerStore is the base store.
            else {
                me._pickerStore = result = store;
            }

            // Bind the picker to the correct store. If it is the default store, this
            // will be a no-op.
            if (picker) {
                picker.setStore(result);
            }
        },

        /**
         * Updates the fields input UI according to the current selection.
         *
         * @param selection
         * @private
         */
        setFieldDisplay: function (selection) {
            var me = this,
                inputValue = '',
                displayTpl;

            if (selection) {
                displayTpl = me.getDisplayTpl();
                if (displayTpl) {
                    inputValue = displayTpl.apply(me.getRecordDisplayData(selection));
                } else {
                    inputValue = selection.get(me.getDisplayField());
                }
            }

            me.setInputValue(inputValue);

            // Ensure clear icon is synced
            me.syncEmptyState();
        }
    }
});
