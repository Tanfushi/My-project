// 表单提交处理
document.getElementById('contact-form').addEventListener('submit', function (event) {
    event.preventDefault(); // 阻止默认表单提交行为
    
    // 获取表单的输入值
    const name = document.getElementById('name').value;
    const email = document.getElementById('email').value;
    const message = document.getElementById('message').value;

    // 验证表单输入是否为空
    if (!name || !email || !message) {
        alert('Please fill in all fields before submitting.');
        return;
    }

    // 假设这是前端处理后的提示，你也可以使用 API 将数据发送到后端
    console.log(`Name: ${name}, Email: ${email}, Message: ${message}`);
    alert('Thank you, ' + name + '! Your message has been sent.');

    // 清空表单内容
    document.getElementById('contact-form').reset();
});

// 添加按钮点击效果
const buttons = document.querySelectorAll('button');

buttons.forEach(button => {
    button.addEventListener('mousedown', () => {
        button.classList.add('active');
    });

    button.addEventListener('mouseup', () => {
        button.classList.remove('active');
    });
});

// 添加页面元素的滚动淡入效果
window.addEventListener('scroll', () => {
    const sections = document.querySelectorAll('.section');
    const scrollPosition = window.scrollY + window.innerHeight;

    sections.forEach(section => {
        if (scrollPosition > section.offsetTop + 100) {
            section.classList.add('visible');
        }
    });
});

