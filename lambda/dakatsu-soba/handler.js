"use strict";
import AWS from 'aws-sdk';
import bcrypt from 'bcryptjs';
import { table } from 'table';
const s3 = new AWS.S3({
  region: "ap-northeast-1"
});

async function getS3Object(
  bucketName,
  objectKey
) {
  try {
    const output = await s3.getObject({
      Bucket: bucketName,
      Key: objectKey
    }).promise();
    return output.Body.toString("utf8");
  } catch (err) {
    throw err
  }
}

const BUCKET = "dakatsu-soba-sheets";

export const get = async (event) => {
  return {
    "isBase64Encoded": false,
    "statusCode": 200,
    "headers": {'Content-Type': 'application/json', 'Access-Control-Allow-Origin': '*'},
    "body": await getS3Object(BUCKET, event.pathParameters.id)
   }
};
export const save = async (event) => {
  let charactor = event.isBase64Encoded ? JSON.parse(Buffer.from(event.body, "base64").toString("utf8")): JSON.parse(event.body);
  if (!charactor.uuid) {
    return {
      "isBase64Encoded": false,
      "statusCode": 400,
      "headers": {},
      "body": JSON.stringify({ "message": "uuidは必須です。" })
     }
  }
  try {
    const content = await getS3Object(BUCKET, charactor.uuid);
    const data = JSON.parse(content);
    // 更新ケース
    if (data.password && charactor.password.trim() !== "") {
      const isPasswordValid = bcrypt.compareSync(charactor.password, data.password);
      if (!isPasswordValid) { 
        return {
          "isBase64Encoded": false,
          "statusCode": 400,
          "headers": {},
          "body": JSON.stringify({ "message": "パスワードが一致しません" })
         }
      }
    }
  } catch (err) {
    // 新規登録ケース
    // パスワードが設定されている場合は、ハッシュ化する
    if (charactor.password && charactor.password.trim() !== "") {
      const salt = bcrypt.genSaltSync(10);
      const hash = bcrypt.hashSync(charactor.password, salt);
      charactor.password = hash;
    }
  }
  await s3.putObject({
    Bucket: BUCKET,
    Key: event.uuid,
    Body: JSON.stringify(charactor)
  });
  delete charactor.password;
  return {
    "isBase64Encoded": false,
    "statusCode": 200,
    "headers": {'Content-Type': 'application/json', 'Access-Control-Allow-Origin': '*'},
    "body": JSON.stringify(charactor)
   }
};
export const output = async (event) => {
  var emptyToSpace = function(s) {
      return s === '' ? ' ' : s;  
  };
  const json = event.isBase64Encoded ? 
    JSON.parse(decodeURIComponent(Buffer.from(event.body, "base64").toString("utf8").substring(6))) : 
    JSON.parse(decodeURIComponent(event.body.substring(6)));
  var config = {
      border: {
          topBody: ``,
          topJoin: ``,
          topLeft: ``,
          topRight: ``,

          bottomBody: ``,
          bottomJoin: ``,
          bottomLeft: ``,
          bottomRight: ``,

          bodyLeft: ``,
          bodyRight: ``,
          bodyJoin: `:`,

          joinBody: ``,
          joinLeft: ``,
          joinRight: ``,
          joinJoin: ``
      },
  };

  var output = '';
  output += '■ パーソナル' + '\r\n';
  output += 'キャラクター名：' + json.profile.name + '\r\n';
  output += '種族：' + json.profile.race + '\r\n';
  output += '享年：' + json.profile.age + '\r\n';
  output += '暗示：' + json.profile.implication + '\r\n';
  output += '身長：' + json.profile.height + '\t体重：' + json.profile.weight + '\r\n';
  output += '髪の色：' + json.profile.hair + '\t瞳の色：' + json.profile.eye + '\t肌の色：' + json.profile.skin + '\r\n';
  var acts = json.tabs.reduce(function(acc1, tab) {
      if (tab.tabType == 'ManeuvaTab') {
          return acc1 + tab.items.reduce(function(acc2, item) {
              return acc2 + (item.act ? new Number(item.act) : 0);
          }, 0);
      }
      return acc1;
  }, 6);
  output += '最大行動値：' + acts + '\r\n';
  output += '\r\n';

  output += '■ 記憶のカケラ' + '\r\n';
  var memories = [];
  memories.push(['名前', '詳細']);
  json.profile.memories.forEach(m => {
      memories.push([emptyToSpace(m.name), emptyToSpace(m.description)]);
  });
  output += table(memories, config).replace(/\n\n/gi, '\n');
  output += '\r\n';

  output += '■ 未練' + '\r\n';
  var regrets = [];
  regrets.push(['対象', '種類', '現在値', '最大値', '発狂効果', '備考']);
  json.profile.regrets.forEach(r => {
      regrets.push([emptyToSpace(r.target), emptyToSpace(r.name), emptyToSpace(r.currentVal), emptyToSpace(r.maxVal), emptyToSpace(r.negative), emptyToSpace(r.description)]);
  });
  output += table(regrets, config).replace(/\n\n/gi, '\n');
  output += '\r\n';

  output += '■ カルマ' + '\r\n';
  var karmas = [];
  karmas.push(['条件', '詳細']);
  json.profile.karmas.forEach(k => {
      karmas.push([emptyToSpace(k.name), emptyToSpace(k.description)]);
  });
  output += table(karmas, config).replace(/\n\n/gi, '\n');
  output += '\r\n';

  output += '■ メモ' + '\r\n';
  output += json.profile.memo + '\r\n';
  output += '\r\n';

  output += '■ ポジション' + '\r\n';
  output += 'ポジション：' + json.classes.positions.map(p => { return p.name }).join('/') + '\r\n';
  output += 'サブポジション：' + json.classes.subPositions.map(p => { return p.name }).join('/') + '\r\n';
  output += 'ハイテック：' + json.classes.highTechs.map(p => { return p.name }).join('/') + '\r\n';
  output += '\r\n';

  output += '■ クラス' + '\r\n';
  var classes = [];
  classes.push(['種別', '取得元', 'クラス名', '個数']);
  json.classes.classes.forEach(c => {
      classes.push([emptyToSpace(c.category), emptyToSpace(c.from), emptyToSpace(c.name), emptyToSpace(c.number)]);
  });
  output += table(classes, config).replace(/\n\n/gi, '\n');
  output += '\r\n';

  output += '■ 強化点' + '\r\n';
  var points = [];
  var busou = 0;
  var heni = 0;
  var kaizou = 0;
  points.push(['クラス名', '武装', '変異', '改造']);
  json.classes.points.forEach(p => {
      busou += p.busou;
      heni += p.heni;
      kaizou += p.kaizou;
      points.push([emptyToSpace(p.name), p.busou, p.heni, p.kaizou]);
  });
  points.push(['総計', busou, heni, kaizou]);
  output += table(points, config).replace(/\n\n/gi, '\n');
  output += '\r\n';

  json.tabs.forEach(t => {
      output += '■ ' + t.title + '\r\n';
      if (t.tabType == 'ManeuvaTab') {
          var maneuvas = [];
          maneuvas.push(['悪意', '部位', 'カテゴリー', '種別', 'マニューバ', 'タイミング', 'コスト', '射程', '効果']);
          t.items.forEach(i => {
              maneuvas.push([i.malice, emptyToSpace(i.region), emptyToSpace(i.maneuvaType), emptyToSpace(i.category), emptyToSpace(i.name), emptyToSpace(i.timing), emptyToSpace(i.cost), emptyToSpace(i.range), emptyToSpace(i.description)]);
          });
          output += table(maneuvas, config).replace(/\n\n/gi, '\n');
          output += '\r\n';
      } else if (t.tabType == 'ResourceTab') {
          var resources = [];
          resources.push(['名前', '説明']);
          t.items.forEach(i => {
              resources.push([emptyToSpace(i.name), emptyToSpace(i.description)]);
          });
          output += table(resources, config).replace(/\n\n/gi, '\n');
          output += '\r\n';
      }
  });
  return {
    "isBase64Encoded": false,
    "statusCode": 200,
    "headers": {'Content-Type': 'text/plain;charset=utf-8', 'Access-Control-Allow-Origin': '*'},
    "body": output
   }
};