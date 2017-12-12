/**
 * A checkable menu item that participates in a `group` of mutually exclusive items.
 *
 * Radio items must be a assigned to a `{@link #cfg!group group}` and only one member of
 * that group is allowed to be checked. The owning `{@link Ext.menu.Menu menu}` provides
 * the `{@link Ext.menu.Menu#cfg!groups groups}` config to assist in managing the state
 * of its radio items.
 *
 *      @example
 *      Ext.Viewport.add({
 *          xtype: 'container',
 *          items: [{
 *              xtype: 'button',
 *              bind: 'Call {menuGroups.option}',
 *               
 *              viewModel: {
 *                  data: {
 *                      menuGroups: {
 *                          option: 'home'
 *                      }
 *                  }
 *              },
 *               
 *              menu: {
 *                  bind: {
 *                      groups: '{menuGroups}'
 *                  },
 *                  items: [{
 *                      text: 'Home',
 *                      group: 'option',  // causes Menu to create this class of item
 *                      value: 'home'
 *                  }, {
 *                      text: 'Work',
 *                      group: 'option',
 *                      value: 'work'
 *                  }, {
 *                      text: 'Mobile',
 *                      group: 'option',
 *                      value: 'mobile'
 *                  }]
 *              }
 *          }]
 *      });
 *
 * @since 6.5.0
 */
Ext.define('Ext.menu.RadioItem', {
    extend: 'Ext.menu.CheckItem',
    alias: 'widget.menuradioitem',

    classCls: Ext.baseCSSPrefix + 'menuradioitem',

    nameable: true,
    shareableName: true,

    ariaRole: 'menuitemradio',

    /**
     * @cfg {String} name
     * This config is used internally by the {@link #cfg!group} config and should not be set.
     */

    config: {
        /**
         * @cfg {String} group (required)
         * Name of a radio group that the item belongs.
         *
         * This assigns a common name to several RadioItems to allow selection of a single value.
         *
         * Note that the group name is local to the owning Menu.
         */
        group: null,

        /**
         * @cfg {Boolean} [allowUncheck=false]
         * By default, as in native RadioGroups, a checked radio item cannot be unchecked
         * by the UI. Set this to `true` to allow unchecking of checked RadioItems.
         */
        allowUncheck: null
    },

    //<debug>
    initialize: function() {
        if (!this.getGroup()) {
            Ext.raise('Menu RadioItems must be configured with a group');
        }
        this.callParent();
    },
    //</debug>

    privates: {
        onSpace: function (e) {
            // Veto uncheck for radio items.
            if (this.checkboxElement.dom.checked) {
                e.preventDefault();
            }
        },

        updateGroup: function (group) {
            // Inheritable will update the NameHolder upon add.
            this.name = group;
        },

        onCheckboxChange: function () {
            var checkboxElement = this.checkboxElement.dom,
                isChecked = checkboxElement.checked;

            // If the DOM is insync with the config state, we are good, there's nothng to do.
            if (isChecked === this.getChecked() || this.getDisabled()) {
                return;
            }

            // The change event only fires in response to UI changes.
            // And the UI is not allowed to UNcheck radio items.
            // So immediately reverse the setting before the event propagates.
            // We do not take over the click event, and control programatically  because:
            // 1. We want interaction to be native wherever possible for accessibility reasons.
            // 2. The click events fires after the change on some platforms so we have no control.
            // 3. We'd also have to handle keystroke accessibility.
            if (!isChecked && !this.getAllowUncheck()) {
                checkboxElement.checked = true;
            }
            // Sync our widget state with the reality of the accessible checkbox field.
            else {
                this.callParent();
            }
        },

        onCheckChange: function () {
            var me = this,
                checkboxElement = me.checkboxElement.dom,
                parentMenu = me.getParent(),
                name, groups, siblings, len, i;

            // Forces the group config to be read and pushed into the name property
            me.getGroup();
            name = me.name;

            // Sync state of all siblings in group via the parent menu *before* we call parent.
            // State must be correct.
            if (parentMenu && name) {
                groups = {};

                if (checkboxElement.checked) {
                    groups[name] = me.getValue();
                    parentMenu.setGroups(groups);
                }
                // Now we have to see if our group has become all unchecked
                // and potentially declare our group value as null.
                else {
                    siblings = parentMenu.lookupName(name);
                    len = siblings && siblings.length;
                    
                    for (i = 0; i < len && !siblings[i].checkboxElement.dom.checked; i++) {
                         // just loop
                    }

                    // If we ran out the end of the loop without finding a check item,
                    // or we are in the config phase, and the menu has no items by that name,
                    // we set the group's value to null.
                    // If we are in the config phase, the parent menu just accumulates the
                    // group values silently, it does not fire the groupchange event, or
                    // publish the groups object.
                    if (i === len) {
                        groups[name] = null;
                        parentMenu.setGroups(groups);
                    }
                }
            }

            me.callParent();
        }
    }
});
