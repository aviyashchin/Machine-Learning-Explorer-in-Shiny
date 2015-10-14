angular.module('appRoutes', []).config(['$routeProvider', '$locationProvider', function($routeProvider, $locationProvider) {

    $routeProvider

        // home page
        .when('/', {
            templateUrl: 'views/index.html',
            controller: 'MainController'
        });

    $locationProvider.html5Mode(true);

}]);