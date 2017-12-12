topSuite("Ext.route.Route", ['Ext.app.Controller'], function () {
    var actionExecuted = false,
        beforeExecuted = false,
        numArgs = 0,
        numBeforeArgs = 0,
        token = 'foo/bar',
        controller,
        beforeSpy, beforeBlockSpy, actionSpy;

    function promiseHasBeenResolved (promise) {
        var resolved = spyOn({
                test: Ext.emptyFn
            }, 'test'),
            rejected = spyOn({
                test: Ext.emptyFn
            }, 'test');

        promise.then(resolved, rejected);

        waitsForSpy(resolved, 'Promise was never resolved');

        runs(function () {
            expect(resolved).toHaveBeenCalled();
            expect(rejected).not.toHaveBeenCalled();
        });
    }

    function promiseHasBeenRejected (promise) {
        var resolved = spyOn({
                test: Ext.emptyFn
            }, 'test'),
            rejected = spyOn({
                test: Ext.emptyFn
            }, 'test');

        promise.then(resolved, rejected);

        waitsForSpy(rejected, 'Promise was never rejected');

        runs(function () {
            expect(rejected).toHaveBeenCalled();
            expect(resolved).not.toHaveBeenCalled();
        });
    }

    beforeEach(function () {
        controller = new Ext.app.Controller({
            beforeHandleRoute: function () {
                numBeforeArgs += arguments.length;
                beforeExecuted = true;

                var action = arguments[arguments.length - 1];

                action.resume();
            },

            beforeHandleRouteBlock: function () {
                numBeforeArgs += arguments.length;
                beforeExecuted = true;

                var action = arguments[arguments.length - 1];

                action.stop(); //stop the current route
            },

            handleRoute: function () {
                numArgs = arguments.length;
                actionExecuted = true;
            }
        });

        beforeSpy = spyOn(controller, 'beforeHandleRoute').andCallThrough();
        beforeBlockSpy = spyOn(controller, 'beforeHandleRouteBlock').andCallThrough();
        actionSpy = spyOn(controller, 'handleRoute').andCallThrough();
    });

    afterEach(function () {
        controller = beforeSpy = beforeBlockSpy = actionSpy =
            actionExecuted = beforeExecuted = Ext.destroy(controller);

        numArgs = numBeforeArgs = 0;
    });

    describe("should recognize tokens", function () {
        it("recognize 'foo/bar'", function () {
            var route = new Ext.route.Route({
                controller : controller,
                action     : 'handleRoute',
                url        : token
            });

            expect(route.recognize(token)).toBeTruthy();
        });

        describe("optional parameters", function () {
            it("recognize 'foo/:id'", function () {
                //:id is a param
                var route = new Ext.route.Route({
                    controller : controller,
                    action     : 'handleRoute',
                    url        : 'foo/:id'
                });

                expect(route.recognize('foo/123')).toBeTruthy();
            });

            it("recognize 'foo/:id' using condition for :id", function () {
                var route = new Ext.route.Route({
                    controller : controller,
                    action     : 'handleRoute',
                    url        : 'foo:id',
                    conditions : {
                        //makes :id param optional
                        ':id' : '(?:(?:/){1}([%a-zA-Z0-9\-\_\s,]+))?'
                    }
                });

                expect(route.recognize('foo/123')).toBeTruthy();
            });
        });
    });

    describe("fire action", function () {
        it("should fire action", function () {
            var route = new Ext.route.Route({
                    url: token,
                    handlers: [
                        {
                            action: 'handleRoute',
                            scope: controller
                        }
                    ]
                }),
                recognized = route.recognize(token),
                promise = route.execute(token, recognized);

            promiseHasBeenResolved(promise);

            runs(function () {
                expect(actionExecuted).toEqual(true);
            });
        });

        it("should fire action using caseInsensitve", function () {
            var route = new Ext.route.Route({
                    url: token,
                    caseInsensitive: true,
                    handlers: [
                        {
                            action: 'handleRoute',
                            scope: controller
                        }
                    ]
                }),
                recognized = route.recognize(token),
                promise = route.execute(token, recognized);

            promiseHasBeenResolved(promise);

            runs(function () {
                expect(actionExecuted).toEqual(true);
            });
        });
    });

    describe("handle before action", function () {
        it("should continue action execution", function () {
            var route = new Ext.route.Route({
                    url: token,
                    handlers: [
                        {
                            action: 'handleRoute',
                            before: 'beforeHandleRoute',
                            scope: controller
                        }
                    ]
                }),
                recognized = route.recognize(token),
                promise = route.execute(token, recognized);

            promiseHasBeenResolved(promise);

            runs(function () {
                expect(beforeExecuted && actionExecuted).toEqual(true);
            });
        });

        it("should block action execution", function () {
            var route = new Ext.route.Route({
                    url: token,
                    handlers: [
                        {
                            action: 'handleRoute',
                            before: 'beforeHandleRouteBlock',
                            scope: controller
                        }
                    ]
                }),
                recognized = route.recognize(token),
                promise = route.execute(token, recognized);

            promiseHasBeenRejected(promise);

            runs(function () {
                expect(beforeExecuted && !actionExecuted).toEqual(true);
            });
        });
    });

    describe("number of arguments", function () {
        it("with a before action", function () {
            var route = new Ext.route.Route({
                    url: 'foo/:bar',
                    handlers: [
                        {
                            action: 'handleRoute',
                            before: 'beforeHandleRoute',
                            scope: controller
                        }
                    ]
                }),
                recognized = route.recognize(token),
                promise = route.execute(token, recognized);

            promiseHasBeenResolved(promise);

            runs(function () {
                expect(numBeforeArgs + numArgs).toBe(3);
            });
        });

        it("without a before action", function () {
            var route = new Ext.route.Route({
                    url: 'foo/:bar',
                    handlers: [
                        {
                            action: 'handleRoute',
                            scope: controller
                        }
                    ]
                }),
                recognized = route.recognize(token),
                promise = route.execute(token, recognized);

            promiseHasBeenResolved(promise);

            runs(function () {
                expect(numBeforeArgs + numArgs).toBe(1);
            });
        });
    });

    describe("controller activity", function () {
        it("should not execute if the controller is inactive", function () {
            var route = new Ext.route.Route({
                    url: token,
                    handlers: [
                        {
                            action: 'handleRoute',
                            scope: controller
                        }
                    ]
                }),
                recognize = route.recognize(token),
                promise;

            controller.deactivate();

            promise = route.execute(token, recognize);

            promiseHasBeenResolved(promise);

            runs(function () {
                expect(actionExecuted).toBeFalsy();
            });
        });

        it("should recognize if the controller is inactive & the allowInactive flag is set", function () {
            var route = new Ext.route.Route({
                    url: token,
                    allowInactive: true,
                    handlers: [
                        {
                            action: 'handleRoute',
                            scope: controller
                        }
                    ]
                }),
                recognize = route.recognize(token),
                promise;

            controller.deactivate();

            promise = route.execute(token, recognize);

            promiseHasBeenResolved(promise);

            runs(function () {
                expect(actionExecuted).toBeTruthy();
            });
        });
    });

    describe("beforeroute event", function () {
        var route;

        beforeEach(function () {
            route = new Ext.route.Route({
                url: token,
                handlers: [
                    {
                        action: 'handleRoute',
                        scope: controller
                    }
                ]
            });
        });

        afterEach(function () {
            if (route) {
                route.destroy();
            }
        });

        describe("using Ext.on", function () {
            it("should fire event", function () {
                var fn = spyOn({
                        test: Ext.emptyFn
                    }, 'test'),
                    recognize = route.recognize(token),
                    promise;

                Ext.on('beforeroute', fn, null, { single: true });

                promise = route.execute(token, recognize);

                promiseHasBeenResolved(promise);

                runs(function () {
                    expect(fn).toHaveBeenCalled();
                });
            });

            it("should execute before added in event listener", function () {
                var fn = spyOn({
                        test: function (action) {
                            action.resume();
                        }
                    }, 'test').andCallThrough(),
                    recognize = route.recognize(token),
                    promise;

                Ext.on('beforeroute', function (action) {
                    action.before(fn);
                }, null, { single: true });

                promise = route.execute(token, recognize);

                promiseHasBeenResolved(promise);

                runs(function () {
                    expect(fn).toHaveBeenCalled();
                });
            });

            it("should execute action added in event listener", function () {
                var fn = spyOn({
                        test: Ext.emptyFn
                    }, 'test'),
                    recognize = route.recognize(token),
                    promise;

                Ext.on('beforeroute', function (action) {
                    action.action(fn);
                }, null, { single: true });

                promise = route.execute(token, recognize);

                promiseHasBeenResolved(promise);

                runs(function () {
                    expect(fn).toHaveBeenCalled();
                });
            });

            it("should not execute before if return false", function () {
                var fn = spyOn({
                        test: function (action) {
                            action.resume();
                        }
                    }, 'test').andCallThrough(),
                    recognize = route.recognize(token),
                    promise;

                Ext.on('beforeroute', function (action) {
                    action.before(fn);

                    return false;
                }, null, { single: true });

                promise = route.execute(token, recognize);

                promiseHasBeenRejected(promise);

                runs(function () {
                    expect(fn).not.toHaveBeenCalled();
                });
            });

            it("should not execute action if return false", function () {
                var fn = spyOn({
                        test: Ext.emptyFn
                    }, 'test'),
                    recognize = route.recognize(token),
                    promise;

                Ext.on('beforeroute', function (action) {
                    action.action(fn);

                    return false;
                }, null, { single: true });

                promise = route.execute(token, recognize);

                promiseHasBeenRejected(promise);

                runs(function () {
                    expect(fn).not.toHaveBeenCalled();
                });
            });
        });

        describe("using event domain in controller", function () {
            var controller;

            afterEach(function () {
                if (controller) {
                    controller.destroy();
                    controller = null;
                }
            });

            it("should be listenable", function () {
                var controller = new Ext.app.Controller({
                        listen: {
                            global: {
                                beforeroute: 'onBeforeRoute'
                            }
                        },

                        onBeforeRoute: Ext.emptyFn
                    }),
                    fn = spyOn(controller, 'onBeforeRoute'),
                    recognize = route.recognize(token),
                    promise = route.execute(token, recognize);

                promiseHasBeenResolved(promise);

                runs(function () {
                    expect(fn).toHaveBeenCalled();
                });
            });

            it("should execute before added in event listener", function () {
                var fn = spyOn({
                        test: function (action) {
                            action.resume();
                        }
                    }, 'test').andCallThrough(),
                    controller = new Ext.app.Controller({
                        listen: {
                            global: {
                                beforeroute: 'onBeforeRoute'
                            }
                        },

                        onBeforeRoute: function (action) {
                            action.before(fn);
                        }
                    }),
                    recognize = route.recognize(token),
                    promise = route.execute(token, recognize);

                promiseHasBeenResolved(promise);

                runs(function () {
                    expect(fn).toHaveBeenCalled();
                });
            });

            it("should execute action added in event listener", function () {
                var fn = spyOn({
                        test: Ext.emptyFn
                    }, 'test'),
                    controller = new Ext.app.Controller({
                        listen: {
                            global: {
                                beforeroute: 'onBeforeRoute'
                            }
                        },

                        onBeforeRoute: function (action) {
                            action.action(fn);
                        }
                    }),
                    recognize = route.recognize(token),
                    promise = route.execute(token, recognize);

                promiseHasBeenResolved(promise);

                runs(function () {
                    expect(fn).toHaveBeenCalled();
                });
            });

            it("should not execute action when an added before stops the action", function () {
                var fn = spyOn({
                        test: Ext.emptyFn
                    }, 'test'),
                    controller = new Ext.app.Controller({
                        listen: {
                            global: {
                                beforeroute: 'onBeforeRoute'
                            }
                        },

                        onBeforeRoute: function (action) {
                            action
                                .before(function (action) {
                                    action.stop();
                                })
                                .action(fn);
                        }
                    }),
                    recognize = route.recognize(token),
                    promise = route.execute(token, recognize);

                promiseHasBeenRejected(promise);

                runs(function () {
                    expect(fn).not.toHaveBeenCalled();
                });
            });

            it("should not execute before if return false", function () {
                var fn = spyOn({
                        test: function (action) {
                            action.resume();
                        }
                    }, 'test').andCallThrough(),
                    controller = new Ext.app.Controller({
                        listen: {
                            global: {
                                beforeroute: 'onBeforeRoute'
                            }
                        },

                        onBeforeRoute: function (action) {
                            action.before(fn);

                            return false;
                        }
                    }),
                    recognize = route.recognize(token),
                    promise = route.execute(token, recognize);

                promiseHasBeenRejected(promise);

                runs(function () {
                    expect(fn).not.toHaveBeenCalled();
                });
            });

            it("should not execute action if return false", function () {
                var fn = spyOn({
                        test: Ext.emptyFn
                    }, 'test'),
                    controller = new Ext.app.Controller({
                        listen: {
                            global: {
                                beforeroute: 'onBeforeRoute'
                            }
                        },

                        onBeforeRoute: function (action) {
                            action.action(fn);

                            return false;
                        }
                    }),
                    recognize = route.recognize(token),
                    promise = route.execute(token, recognize);

                promiseHasBeenRejected(promise);

                runs(function () {
                    expect(fn).not.toHaveBeenCalled();
                });
            });
        });
    });

    describe('removeHandler', function () {
        var controller2;

        afterEach(function () {
            if (controller2) {
                controller2.destroy();
                controller2 = null;
            }
        });

        it('should remove handler passing just the scope', function () {
            controller2 = new Ext.app.Controller({
                handleRoute: Ext.emptyFn
            });

            var route = new Ext.route.Route({
                url: 'foo',
                handlers: [
                    {
                        action: 'handleRoute',
                        scope: controller
                    },
                    {
                        action: 'handleRoute',
                        scope: controller2
                    }
                ]
            });

            expect(route.getHandlers().length).toBe(2);

            route.removeHandler(controller2);

            expect(route.getHandlers().length).toBe(1);

            expect(route.getHandlers()[0].scope).toBe(controller);
        });

        xit('should remove handler passing scope and which handler', function () {
            controller2 = new Ext.app.Controller({
                handleRoute: Ext.emptyFn
            });

            var handler = new Ext.route.Handler({
                    action: 'handleRoute',
                    scope: controller2
                }),
                route = new Ext.route.Route({
                    url: 'foo',
                    handlers: [
                        {
                            action: 'handleRoute',
                            scope: controller
                        },
                        {
                            action: 'handleRoute',
                            scope: controller2
                        },
                        handler
                    ]
                });

            expect(route.getHandlers().length).toBe(3);

            route.removeHandler(controller2, handler);

            expect(route.getHandlers().length).toBe(2);

            expect(route.getHandlers()[0].scope).toBe(controller);
            expect(route.getHandlers()[1].scope).toBe(controller2);
        });
    });
});
