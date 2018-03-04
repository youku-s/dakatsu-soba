'use strict';

var aws = require('aws-sdk');
var docClient = new aws.DynamoDB.DocumentClient({region: 'ap-northeast-1'});

exports.handler = function (event, context, callback) {
  const params = {
    TableName : 'dakatsu-sheet',
    Key: {
        'uuid': event['id']
    }
  };

   docClient.get(params, function(err, data) {
        if (err){
            callback(new Error("取得に失敗しました。"));
        } else {
            context.succeed(data.Items);
        }
    });
};