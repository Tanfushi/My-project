const express = require('express');
const bodyParser = require('body-parser');
const { DynamoDBClient } = require('@aws-sdk/client-dynamodb'); // 导入 DynamoDBClient
const { PutCommand, DynamoDBDocumentClient } = require('@aws-sdk/lib-dynamodb'); // 导入 PutCommand 和 DocumentClient

const app = express();
const port = 3001;

// 初始化 DynamoDB 客户端
const client = new DynamoDBClient({ region: 'us-east-1' }); // 替换为你的 AWS 区域
const dynamoDB = new DynamoDBClient({ region: 'us-east-1' }); // 替换为你的 DynamoDB 表所在的区域


// 中间件，用于解析请求体中的 JSON 数据
app.use(bodyParser.json());

app.use((req, res, next) => {
  res.header('Access-Control-Allow-Origin', '*');
  res.header('Access-Control-Allow-Headers', 'Origin, X-Requested-With, Content-Type, Accept');
  next();
});

// 定义 POST 路由，处理表单提交并存储到 DynamoDB
app.post('/contact', async (req, res) => {
  const { name, email, message } = req.body;
  console.log('Received form data:', { name, email, message }); // 调试日志

  if (!name || !email || !message) {
    console.error('Form validation failed. Missing name, email, or message.');
    return res.status(400).send('Please provide name, email, and message.');
  }

  // 配置 DynamoDB 插入操作
  const params = {
    TableName: 'ContactMessage', // 替换为你的 DynamoDB 表名
    Item: {
      id: Date.now().toString(), // 用时间戳生成唯一 ID
      name,
      email,
      message,
    },
  };

  try {
    const putCommand = new PutCommand(params); // 使用 PutCommand
    const data = await dynamoDB.send(putCommand); // 使用 send() 方法发送命令
    console.log('Data saved to DynamoDB:', data); // 打印成功保存日志
    res.status(200).send('Message saved successfully!');
  } catch (err) {
    console.error('Error saving data to DynamoDB:', err);
    res.status(500).send('Internal Server Error');
  }
});

// 启动服务器
app.listen(port, () => {
  console.log(`Server running at http://localhost:${port}`);
});



