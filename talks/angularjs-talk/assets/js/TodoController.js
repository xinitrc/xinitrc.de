var app = angular.module('Todo');

var todoCtrl = function ($scope){
   $scope.todos = [];

   $scope.addTodo = function () {
       $scope.todos.push ({text: $scope.todotext, done: false});
       $scope.todotext = "";
   }

   $scope.remaining = function () {
       var count = 0;
       angular.forEach ($scope.todos, function (each) { if (! each.done) {count++;}  })
       return count;
   }

   $scope.updateTodo = function(todo) {
       todo.done = true;
   }

   $scope.clearDone = function() {
       var oldTodos = $scope.todos;
       $scope.todos = [];
       angular.forEach(oldTodos, function(todo) {
           if (!todo.done) {
               $scope.todos.push(todo);
           }
       })
   }
};


todoCtrl.$inject = ['$scope'];

app.controller("TodoCtrl", todoCtrl);

