'use strict';

var aws = require('aws-sdk');
var docClient = new aws.DynamoDB.DocumentClient({region: 'ap-northeast-1'});

exports.handler = function (event, context) {
    if (event.uuid === null || event.uuid === undefined) {
        context.fail("uuidは必須です。");
    }

    var params = {
        TableName: 'dakatsu-sheet',
        Item: event
    };

    docClient.put(params, function (err, data) {
        if (err) {
            context.fail(err);
        }
        context.succeed({
            "statusCode": 200,
            "headers": {'Content-Type': 'application/json', 'Access-Control-Allow-Origin': '*'},
            "body": ""
        });
    });
};