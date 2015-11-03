<?php

/*********************************************************
* Eworx S.A.- 2012
* @Author kp@eworx.gr
* EFWEB-596 R Resources Management Layer (RRML)
* provides a DB API related to the RRML
**********************************************************/

	require_once('settings.php');

	/**
	 * gets a database connection based on the settings.php
	 * @return MysqliDb a new MysqliDb instance
	 */

	function getConnection(){
		global $db_host;
		global $db_user;
		global $db_pass;
		global $db_name;

		return  mysqli_connect($db_host, $db_user, $db_pass, $db_name);
	}

	#------ SERVICE STATUS -------------------------------
	#------ GET -------------------------------

	/**
	 * The request is performed in a synchronized manner
	 * @return int the status of the services 0 = stopped -1 = initializing 1 = initialized
	 */

	function getServiceStatus(){
		$result = null;
		if ($mysqli = getConnection()){
			$query = "SELECT status FROM service_status FOR UPDATE;";
			$queryResult = $mysqli -> query($query);
			$result =  mysqli_fetch_row( $queryResult );
			$result = (int) $result[0];
			mysqli_free_result($queryResult);
			mysqli_close($mysqli);
		}
		return $result;
	}

	/**
	 * The request is performed in a synchronized manner
	 * Using screen we can determine the actuall status.
	 * @return boolean if the status in the db indicates that the service is initiated
	 */

	function isServiceInitiated(){
		return getServiceStatus() == 1 ;
	}

	/**
	 * The request is performed in a synchronized manner
	 * @return boolean if the status in the db indicates that the service is initiating
	 */

	function isServiceInitiating(){
		return getServiceStatus() == -1 ;
	}

	/**
	 * Note that this functions cannot be used to fully determine the status of the service
	 * Using screen we can determine the actuall status.
	 * The request is performed in a synchronized manner
	 * @return boolean if the status in the db indicates that the service is stopped
	 */

	function isServiceStopped(){
		return getServiceStatus() == 0 ;
	}

	#------ SERVICE STATUS -------------------------------
	#------ SET -------------------------------

	/**
	 * set the service status
	 */

	function setServiceStatus($status){
		if ($mysqli = getConnection()){
			$query = "UPDATE service_status SET status = ?";
			$stmt = mysqli_prepare($mysqli, $query);
			mysqli_stmt_bind_param($stmt, 'i', $status);
			mysqli_stmt_execute($stmt);
			mysqli_stmt_close($stmt);
			mysqli_close($mysqli);
		}
	}

	//setServiceStatus(0);
	//var_dump(getServiceStatus());

	/**
	 * set the service status to 1
	 * @return the number of affected rows
	 */

	function serviceInitiated(){
		return setServiceStatus(1);
	}

	/**
	 * set the service status to -1
	 * @return the number of affected rows
	 */

	function serviceInitiating(){
		return setServiceStatus(-1);
	}

	/**
	 * set the service status to 0
	 * @return the number of affected rows
	 */

	function serviceStoped(){
		return setServiceStatus(0);
	}

	#------ R INSTANCES STATUS -------------------------------
	#------ SET  -------------------------------


	function initializingRInstance($rInstanceName){
		if ($mysqli = getConnection()){
			$status = -1;
			$query = "INSERT INTO r_instances_status VALUES (?, ?)";
			$stmt = mysqli_prepare($mysqli, $query);
			mysqli_stmt_bind_param($stmt, 'si', $rInstanceName, $status);
			mysqli_stmt_execute($stmt);
			mysqli_stmt_close($stmt);
			mysqli_close($mysqli);
		}
	}

	//initializingRInstance("zzz");

	function setRInstanceStatus($rInstanceName, $status){
		if ($mysqli = getConnection()){
			$query = "UPDATE r_instances_status SET status = ? where id = ?";
			$stmt = mysqli_prepare($mysqli, $query);
			mysqli_stmt_bind_param($stmt, 'is', $status, $rInstanceName);
			mysqli_stmt_execute($stmt);
			mysqli_stmt_close($stmt);
			mysqli_close($mysqli);
		}
	}

	//initializingRInstance("zzz");
	//setRInstanceStatus('zzz', 0);
	//var_dump(getRInstancesStatus());

	function setAvailableRInstance($rInstanceName){
		setRInstanceStatus($rInstanceName, 1);
	}

	function setUsedRInstance($rInstanceName){
		setRInstanceStatus($rInstanceName, 0);
	}

	function deleteRInstance($rInstanceName){
		if ($mysqli = getConnection()){
			$query = "DELETE from r_instances_status WHERE id = ?";
			$stmt = mysqli_prepare($mysqli, $query);
			mysqli_stmt_bind_param($stmt, 's', $rInstanceName);
			mysqli_stmt_execute($stmt);
			mysqli_stmt_close($stmt);
			mysqli_close($mysqli);
		}
	}

	//deleteRInstance("zzz");
	//var_dump(getRInstancesStatus());
	//initializingRInstance("zzz");
	//var_dump(getRInstancesStatus());
	//deleteRInstance("zzz");
	//var_dump(getRInstancesStatus());

	function deleteRInstances(){
		if ($mysqli = getConnection()){
			$query = "DELETE from r_instances_status";
			$stmt = mysqli_prepare($mysqli, $query);
			mysqli_stmt_execute($stmt);
			mysqli_stmt_close($stmt);
			mysqli_close($mysqli);
		}
	}

	//deleteRInstances();
	//var_dump(getRInstancesStatus());
	//initializingRInstance("zzz");
	//initializingRInstance("zzz2");
	//var_dump(getRInstancesStatus());
	//deleteRInstances();
	//var_dump(getRInstancesStatus());

	#------ R INSTANCES STATUS -------------------------------
	#------ GET  -------------------------------

	function getRInstancesStatus(){
		$result = NULL;
		if ($mysqli = getConnection()){
			$query = "SELECT * FROM r_instances_status FOR UPDATE";
			$stmt = $mysqli -> prepare($query);
			$stmt -> execute();
			$result = fetch($stmt);
			mysqli_stmt_close($stmt);
			mysqli_close($mysqli);
		}
		return $result;
	}

  //var_dump(getRInstancesStatus());

	/**
	 * The request is performed in a synchronized manner
	 * @return int the status of the rInstanceName 0 = stopped -1 = initializing 1 = initialized
	 */

	function getRInstanceStatus($rInstanceName){
		$result = NULL;
		if ($mysqli = getConnection()){
			$query = "SELECT status FROM r_instances_status WHERE id = ? FOR UPDATE";
			$stmt = $mysqli -> prepare($query);
			$stmt -> bind_param("s", $rInstanceName);
			$stmt -> execute();
			$result = fetch($stmt);
			$result = (int) $result[0]['status'];
			mysqli_stmt_close($stmt);
			mysqli_close($mysqli);
		}
		return $result;
	}

	//initializingRInstance("zzz");
	//var_dump(getRInstanceStatus("zzz"));


	function isRInstanceAvailable($rInstanceName){
		return getRInstanceStatus($rInstanceName) == 1;
	}

	function isRInstanceUsed($rInstanceName){
		return !isRInstanceAvailable();
	}

	function getRInstancesByStatus($status){
		$result = NULL;
		if ($mysqli = getConnection()){
			$query = "SELECT id FROM r_instances_status WHERE status = ? FOR UPDATE";
			$stmt = $mysqli -> prepare($query);
			$stmt -> bind_param("i", $status);
			$stmt -> execute();
			$result = fetch($stmt);
			mysqli_stmt_close($stmt);
			mysqli_close($mysqli);
		}
		return $result;
	}

	//deleteRInstances();
	//initializingRInstance("zzz");
	//initializingRInstance("zzz2");
	//var_dump(getRInstancesByStatus(1));

	function getAvailableRInstances(){
		return getRInstancesByStatus(1);
	}

	//deleteRInstances();
	//initializingRInstance("zzz");
	//setRInstanceStatus("zzz",1);
	//var_dump(getAvailableRInstances());
	//deleteRInstances();

	function getUsedRInstances(){
		return getRInstancesByStatus(0);
	}


	#-----------------------------------------

	function reserveRInstance(){
		$mysqli = getConnection();

		$mysqli -> autocommit(false); // begin transaction
		$mysqli -> query("SET @availableRInstance = (SELECT id FROM r_instances_status where status = 1 ORDER BY id limit 1 FOR UPDATE);"); // lock table
		$mysqli -> query("UPDATE r_instances_status SET status = 0 WHERE id = @availableRInstance;");
		$result =  mysqli_fetch_row( $mysqli -> query("SELECT @availableRInstance;") );
		$mysqli -> commit(); // end transaction

		return $result[0];
	}

#-----------------------------------------
#-----------------------------------------

	/**
	* fetches all rows from a result set - either normal or prepared.
	* http://www.php.net/manual/en/mysqli-stmt.bind-result.php#102179
	*/

	function fetch($result){
    $array = array();
    if($result instanceof mysqli_stmt){
        $result -> store_result();
        $variables = array();
        $data = array();
        $meta = $result -> result_metadata();
        while($field = $meta->fetch_field())
            $variables[] = &$data[$field -> name]; // pass by reference
        call_user_func_array(array($result, 'bind_result'), $variables);
        $i = 0;
        while($result->fetch()){
            $array[$i] = array();
            foreach($data as $k=>$v)
                $array[$i][$k] = $v;
            $i++;
        }
    }elseif($result instanceof mysqli_result){
        while($row = $result -> fetch_assoc())
            $array[] = $row;
    }
    return $array;
	}
