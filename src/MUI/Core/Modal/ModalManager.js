const ModalManager = require("@material-ui/core/Modal/ModalManager").default;

exports.constructor = function(opts) {
  return function() {
    return new ModalManager(opts);
  };
};

exports.add = function(manager) {
  return function(modal) {
    return function(container) {
      return function() {
        return manager.add(modal, container);
      };
    };
  };
};

exports.remove = function(manager) {
  return function(modal) {
    return function() {
      return manager.remove(modal);
    };
  };
};

exports.isTopModal = function(manager) {
  return function(modal) {
    return manager.isTopMoal(modal);
  };
};
