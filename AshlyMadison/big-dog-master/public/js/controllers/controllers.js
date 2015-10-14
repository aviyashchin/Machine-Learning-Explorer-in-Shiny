var app = angular.module('dogApp', ['ngFileUpload']);

app.controller('DogCtrl', ['$scope', 'Upload', '$timeout', function ($scope, Upload, $timeout) {

    $scope.uploadFiles = function(file) {
        $scope.f = file;
        if (file && !file.$error) {
            file.upload = Upload.upload({
                 url: 'api/user/uploads',
                  method: 'POST',
                  file: file
            });

            file.upload.then(function (response) {
                $timeout(function () {
                    file.result = response.data;
                });
            }, function (response) {
                if (response.status > 0)
                    $scope.errorMsg = response.status + ': ' + response.data;
            });

            file.upload.progress(function (evt) {
                file.progress = Math.min(100, parseInt(100.0 * 
                                                       evt.loaded / evt.total));
            });
        }   
    }
}]);