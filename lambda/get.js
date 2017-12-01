'use strict';

var aws = require('aws-sdk');
var bcrypt = require('bcryptjs');
var docClient = new aws.DynamoDB.DocumentClient({region: 'ap-northeast-1'});

exports.handler = function (event, context, callback) {
    if (event.uuid === null || event.uuid === undefined) {
        context.fail(new Error("uuidは必須です。"));
    }

    const queryParams = {
        TableName: 'dakatsu-sheet',
        FilterExpression : '#u = :val',
        ExpressionAttributeValues : {':val': event['uuid']},
        ExpressionAttributeNames : {'#u': 'uuid'},
    };

    docClient.scan(queryParams, (error, data) => {
        if (error) {
            context.fail(new Error("取得に失敗しました。"));
        } else if (data.Count < 1) {
            if (data.password === null || data.password === "") {
                var putParams = {
                    TableName: 'dakatsu-sheet',
                    Item: event
                };
                docClient.put(putParams, function (err, data) {
                    if (err) {
                        context.fail(err);
                    }
                    context.succeed({
                        "statusCode": 200,
                        "headers": {'Content-Type': 'application/json', 'Access-Control-Allow-Origin': '*'},
                        "body": ""
                    });
                });
            } else {
                // 新規登録ケース
                bcrypt.genSalt(10, function(err, salt) {
                    bcrypt.hash(event.password || "", salt, function(err, hash) {
                        event.password = hash;
                    
                        var putParams = {
                            TableName: 'dakatsu-sheet',
                            Item: event
                        };
                        docClient.put(putParams, function (err, data) {
                            if (err) {
                                context.fail(err);
                            }
                            context.succeed({
                                "statusCode": 200,
                                "headers": {'Content-Type': 'application/json', 'Access-Control-Allow-Origin': '*'},
                                "body": ""
                            });
                        });
                    });
                });
            }
        } else {
            if (data.Items[0].password === null) {
                var putParams = {
                    TableName: 'dakatsu-sheet',
                    Item: event
                };
                docClient.put(putParams, function (err, data) {
                    if (err) {
                        context.fail(err);
                    }
                    context.succeed({
                        "statusCode": 200,
                        "headers": {'Content-Type': 'application/json', 'Access-Control-Allow-Origin': '*'},
                        "body": ""
                    });
                });
            } else {
                // 更新ケース
                bcrypt.compare(event.password || "", data.Items[0].password || "", function(err, res) {
                    if (err) {
                        context.fail(err);
                    }
                    if (res) {
                        var putParams = {
                            TableName: 'dakatsu-sheet',
                            Item: event
                        };
                        docClient.put(putParams, function (err, data) {
                            if (err) {
                                context.fail(err);
                            }
                            context.succeed({
                                "statusCode": 200,
                                "headers": {'Content-Type': 'application/json', 'Access-Control-Allow-Origin': '*'},
                                "body": ""
                            });
                        });
                    } else {
                        context.fail(new Error("パスワードが一致しません。"));
                    }
                });

            }
        }
    });
};