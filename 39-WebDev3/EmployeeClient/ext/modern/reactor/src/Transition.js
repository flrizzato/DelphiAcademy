/**
 * Animates the creation and destruction of child elements.  This component is especially
 * useful for animating transitions between routes with react-router. Child elements
 * should be given unique keys to ensure they are properly replaced (and not merely
 * updated) when changes occur.
 *
 * Here is an example of how to use Transition with react-router to create a slide effect
 * when changing routes:
 *
 *      import React from 'react';
 *      import { HashRouter as Router, Switch, Route, Redirect } from 'react-router-dom';
 *      import { Transition } from '@extjs/ext-react';
 *      import NewsFeed from './NewsFeed';
 *      import Article from './Article';
 *
 *      function Layout() {
 *          return (
 *              <Transition>
 *                  <Switch>
 *                      <Route path="/articles" component={NewsFeed}/>
 *                      <Route path="/articles/:id" component={Article}/>
 *                  </Switch>
 *              </Transition>
 *          )
 *      }
 *
 */
Ext.define('Ext.reactor.Transition', {
    extend: 'Ext.Container',
    xtype: 'transition',

    requires: ['Ext.fx.animation.*'],

    initial: true,

    config: {
        /**
         * @cfg {"slide"/"reveal"/"cover"/"fade"/"pop"} type
         * The type of animation to use.
         */
        type: 'slide',

        /**
         * @cfg {Number}
         * The duration of animations in milliseconds
         */
        duration: 350,

        /**
         * @cfg {String}
         * The easing function to use for animations. Valid values are 'ease', 'linear', 
         * ease-in', 'ease-out', 'ease-in-out', or a cubic-bezier curve as defined by CSS.
         */
        easing: 'ease',

        /**
         * @cfg {String}
         * The direction of the forward animation.
         */
        direction: 'left',

        /**
         * @cfg {Boolean}
         * Automatically switch directions based on browser URL changes. This should
         * generally be set to true when animating transitions based on client-side routing.
         */
        bindDirectionToLocation: true
    },

    statics: {
        __reactorUpdateConfigsBeforeChildren: {
            location: true,
            direction: true
        }
    },

    initialize: function () {
        this.newLocation = location.href;
        this.callParent();
    },

    computeDirection: function () {
        var me = this,
            newLocation, oldLocation;

        if (me.getBindDirectionToLocation()) {
            newLocation = me.newLocation || '';
            oldLocation = me.oldLocation || '';

            if (newLocation.length > oldLocation.length && !newLocation.indexOf(oldLocation)) {
                return 'left';
            }
            if (newLocation.length < oldLocation.length && !oldLocation.indexOf(newLocation)) {
                return 'right';
            }
        }
         
        return me.getDirection();
    },

    // override add to show animation when children are added
    add: function (items) {
        var me = this,
            animations, i;

        if (!Ext.isArray(items)) {
            items = [items];
        }

        animations = me.createAnimations();

        for (i = 0; i < items.length; i++) {
            me.addAnimationConfigs(items[i]);
        }

        me.callParent(arguments);

        if (me.initial) {
            // don't show animation on initial render
            animations.showAnimation = null;
            me.initial = false;
        }

        items.forEach(function (item) { // need a closure for the RAF
            item.setStyle({ visibility: 'visible' });
            item.show(animations.showAnimation);

            // override destroy to first hide then destroy
            var originalDestroy = item.destroy.bind(item);
            item.destroy = me.destroyChild.bind(me, item, originalDestroy);
        });
    },

    insert: function (index, item) {
        // order doesn't matter since we're using a floating layout
        this.add(item);
    },

    destroyChild: function (item, originalDestroy) {
        var me = this,
            loc = location.href,
            direction, hideAnimation, type;
        
        if (item.animatingDestroy) {
            return;
        }

        me.oldLocation = me.newLocation || loc;
        me.newLocation = loc;

        hideAnimation = me.createAnimations().hideAnimation;
        type = me.getType();
        direction = me.computeDirection();

        if (type === 'cover') {
            item.setZIndex(direction === 'left' || direction === 'top' ? 0 : 2);
        } else if (type === 'reveal') {
            item.setZIndex(direction === 'left' || direction === 'top' ? 2 : 0);
        } 

        item.animatingDestroy = true;

        if (item.activeAnimation) {
            item.activeAnimation.stop();
        }

        if (hideAnimation.type === 'reactor-delay') {
            Ext.defer(originalDestroy, hideAnimation.duration);
        } else {
            item.on('hide', originalDestroy);
            item.hide(hideAnimation);
        }
    },

    addAnimationConfigs: function (child) {
        child.setConfig({
            zIndex: 1,
            top: 0,
            left: 0,
            bottom: 0,
            right: 0,
            style: {
                // prevent new view from "flashing" in before animating in safari
                visibility: 'hidden'
            }
        });
    },

    createAnimations: function () {
        var me = this, 
            type = me.getType(),
            duration = me.getDuration(),
            easing = me.getEasing(),
            direction = me.computeDirection();

        if (type === 'reveal') {
            if (direction === 'left' || direction === 'up') {
                return {
                    showAnimation: null,
                    hideAnimation: {
                        type: 'slideOut',
                        easing: easing,
                        direction: direction,
                        duration: duration
                    }
                };
            }

            return {
                showAnimation: {
                    type: 'slideIn',
                    easing: easing,
                    direction: direction,
                    duration: duration
                },
                hideAnimation: {
                    type: 'reactor-delay',
                    duration: duration
                }
            };
        }
        
        if (type === "cover") {
            if (direction === 'left' || direction === 'up') {
                return {
                    showAnimation: {
                        type: 'slideIn',
                        easing: easing,
                        direction: direction,
                        duration: duration
                    },
                    hideAnimation: {
                        type: 'reactor-delay',
                        duration: duration
                    }
                };
            }

            return {
                showAnimation: null,
                hideAnimation: {
                    type: 'slideOut',
                    easing: easing,
                    direction: direction,
                    duration: duration
                }
            };
        }

        return {
            showAnimation: {
                type: type + 'In',
                easing: easing,
                direction: direction,
                duration: duration
            },
            hideAnimation: {
                type: type + 'Out',
                easing: easing,
                direction: direction,
                duration: duration
            }
        };
    }    
});
