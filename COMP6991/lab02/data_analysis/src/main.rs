use csv::ReaderBuilder;
use std::collections::HashMap;
use std::error::Error;
use std::fs::File;
use serde::Deserialize;

#[derive(Debug, Deserialize)]
struct Enrolment {
    #[serde(rename = "0")] course_code: String,
    #[serde(rename = "1")] student_id: String,
    #[serde(rename = "2")] name: String,
    #[serde(rename = "3")] room: String,
    #[serde(rename = "4")] subjects: String,
    #[serde(rename = "5")] grade: f32,
    #[serde(rename = "6")] term: String,
    #[serde(rename = "7")] dob: String,
    #[serde(rename = "8")] gender: String,
}


const ENROLMENTS_PATH: &str = "enrolments.psv";

fn main() -> Result<(), Box<dyn Error>> {
    let number_students = count_number_students(ENROLMENTS_PATH)?;
    println!("Number of students: {}", number_students);

    let (most_common, most_count) = most_common_course(ENROLMENTS_PATH)?;
    println!("Most common course: {} with {} students", most_common, most_count);

    let (least_common, least_count) = least_common_course(ENROLMENTS_PATH)?;
    println!("Least common course: {} with {} students", least_common, least_count);

    let wam = average_wam(ENROLMENTS_PATH)?;
    println!("Average WAM: {:.2}", wam);
    
    Ok(())
}

fn most_common_course(path: &str) -> Result<(String, usize), Box<dyn Error>> {
    let course_counts = count_courses(path)?;

    let (most_common, most_count) = course_counts
        .iter()
        .max_by_key(|entry| entry.1) // max value in map
        .map(|(course, &count)| (course.clone(), count))
        .unwrap_or(("Unknown".to_string(), 0));

    Ok((most_common, most_count))
}

fn least_common_course(path: &str) -> Result<(String, usize), Box<dyn Error>> {
    let course_counts = count_courses(path)?;

    let (least_common, least_count) = course_counts
        .iter()
        .min_by_key(|entry| entry.1) // min value in map
        .map(|(course, &count)| (course.clone(), count))
        .unwrap_or(("Unknown".to_string(), 0));

    Ok((least_common, least_count))
}

/// Helper function for number of courses
fn count_courses(path: &str) -> Result<HashMap<String, usize>, Box<dyn Error>> {
    let file = File::open(path)?;
    let mut rdr = ReaderBuilder::new().delimiter(b'|').has_headers(false).from_reader(file);

    let mut course_counts: HashMap<String, usize> = HashMap::new();

    rdr.deserialize::<Enrolment>()
        .filter_map(|record| record.ok())
        .for_each(|enrolment| {
            *course_counts.entry(enrolment.course_code).or_insert(0) += 1;
        });

    Ok(course_counts)
}

fn count_number_students(path: &str) -> Result<usize, Box<dyn Error>> {
    let file = File::open(path)?;
    let mut rdr = ReaderBuilder::new()
        .delimiter(b'|')
        .has_headers(false)
        .from_reader(file);

    let mut student_count: HashMap<String, usize> = HashMap::new();

    rdr.deserialize::<Enrolment>()
        .filter_map(|record| record.ok())
        .for_each(|enrolment| {
            student_count.entry(enrolment.student_id).or_insert(1);
        });

    Ok(student_count.len())
}

fn average_wam(path: &str) -> Result<f32, Box<dyn Error>> {
    let file = File::open(path)?;
    let mut rdr = ReaderBuilder::new()
        .delimiter(b'|')
        .has_headers(false)
        .from_reader(file);

    let mut student_grades: HashMap<String, (f32, usize)> = HashMap::new();

    // Accumulate grades for each student
    rdr.deserialize::<Enrolment>()
        .filter_map(|record| record.ok())
        .for_each(|enrolment| {
            let entry = student_grades.entry(enrolment.student_id).or_insert((0.0, 0));
            entry.0 += enrolment.grade;
            entry.1 += 1;
        });

    // Calculate total WAM for all students
    let (total_wam, num_students) = student_grades.iter().fold((0.0, 0), |(total_wam, num_students), (_, (total_grade, num_courses))| {
        let student_wam = total_grade / *num_courses as f32;
        (total_wam + student_wam, num_students + 1)
    });

    if num_students == 0 {
        return Err("No students found in the file".into());
    }

    // Calculate the average WAM for all students
    let average_wam = total_wam / num_students as f32;
    Ok((average_wam * 100.0).round() / 100.0) // Round to 2 decimal places
}

