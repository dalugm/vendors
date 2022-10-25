#!/bin/bash

# 这一步表示加载环境变量，因为在 crontab（计划任务）中默认没有环境变量，
# 用于计划任务的脚本建议加上这一句以避免问题
source /etc/profile

# 在 screen 中服务端进程的名称，执行 screen -ls 可以查看
MC_PROCESS='mcserver'

# 服务端根目录，也就是 start.sh 脚本所在路径
MC_PATH='/root/minecraft'

# 变量赋值：这一段是 grep 正则表达式和 awk 从文本中过滤出服务端进程的
# screen 进程号
PID=$(screen -ls | grep -E '[0-9]+\.[[:alnum:]]+' | grep "\.${MC_PROCESS}\>" | awk -F'.' '{print $1}')

# 逻辑判断：如果服务端进程的 screen 进程号不为空，也就是服务端处于启动状态
# 的时候（没在启动状态也就没有必要执行停止服务端操作了）
if [ -n "$PID" ]; then
    # 发送重启提醒
    screen -dr ${MC_PROCESS} -X stuff "say 服务器将在 20 秒后例行重启，请马上下线！\n"
    sleep 10
    screen -dr ${MC_PROCESS} -X stuff "say 服务器将在 10 秒后例行重启，请马上下线！\n"
    sleep 9
    screen -dr ${MC_PROCESS} -X stuff "say 服务器重启中！\n"
    sleep 1
    # 执行 stop 停止服务器
    screen -dr ${MC_PROCESS} -X stuff "stop\n"
    # 计数器，给下面的 while 循环使用
    TIMER=1
    # 休眠 10 秒钟，因为服务器正常停止也需要几秒钟时间
    sleep 10
    # while 循环开始，该循环主要作用是循环检查服务端进程是否还在，如果不
    # 在的话就表示服务端停止成功，再次执行 start.sh 脚本可重新启动服务端，
    # 如果等待很长时间服务端还是没有成功被关掉的话会强制杀掉进程并执行
    # start.sh 完成重启
    while true; do
        # 重新检查服务端进程还在不在
        PID=$(screen -ls | grep -E '[0-9]+\.[[:alnum:]]+' | grep "$MC_PROCESS" | awk -F'.' '{print $1}')
        # 判断服务端 screen 进程是否为空，如果为空则切换到服务端所在的目
        # 录并备份当前世界，然后执行 start.sh 启动服务端，结束 while 循环
        if [ -z "$PID" ]; then
            cd ${MC_PATH} || return
            tar -zcvf ~/backups/world_"$(date +%Y-%m-%d)".tar.gz world
            bash start.sh
            break
        else
            # PID 不为空的话说明服务端没有正常被关闭，那么就进入循环检查
            # 阶段。每次循环休眠时间为 5 秒，每次循环计数器值加 1
            sleep 5
            (( TIMER+=1 ))
            # 计数器当前值大于 20 时说明服务器无法正常关闭。所以执行
            # kill -9 强制杀掉服务端的 java 进程。注意，服务端的 java 进
            # 程号和其 screen 进程号之间的关系是：java 进程号比 screen
            # 进程号大 1
            if [ $TIMER -gt 20 ]; then
                PID_PLUS=$(( PID+1 ))
                kill -9 "$PID_PLUS"
                sleep 3
                # 一般情况下 screen 中运行的服务端 java 进程被结束后相应
                # 的 screen 进程也会结束，但是以防万一这里又再次判断一下
                # screen 的进程是否还在，如果还在的话也执行kill -9 强制
                # 杀掉进程
                PID_EX=$(ps aux | pgrep "${PID}" | grep -v grep)
                if [ -n "$PID_EX" ]; then
                    kill -9 "$PID"
                fi
                # 强制杀掉进程一般是瞬间完成的，所以在强制杀掉进程后休眠
                # 3 秒（保险起见至少设置 1 秒，理论上不设置也行）便可以
                # 切换到服务端根目录，备份世界并启动服务器端
                sleep 3
                cd ${MC_PATH} || return
                tar -zcvf ~/backups/world_"$(date +%Y-%m-%d)".tar.gz world
                bash start.sh
                break
            fi
        fi
    done
fi
