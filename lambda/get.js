'use strict';

var aws = require('aws-sdk');
var docClient = new aws.DynamoDB.DocumentClient({region: 'ap-northeast-1'});

exports.handler = function (event, context, callback) {
    const params = {
        TableName: 'dakatsu-sheet',
        FilterExpression : '#u = :val',
        ExpressionAttributeValues : {':val': event['id']},
        ExpressionAttributeNames : {'#u': 'uuid'},
    };

    docClient.scan(params, (error, data) => {
        if (error) {
            callback(new Error("取得に失敗しました。"));
        } else if (data.Count < 1) {
            callback(new Error("取得に失敗しました。"));
        } else {
            context.succeed(data.Items[0]);
        }
    });
};